//! Remote command handling module.
use bevy_app::{App, PreStartup};
use bevy_ecs::{
    system::{Command, Commands, In, IntoSystem, ResMut},
    world::World,
};
use core::any::TypeId;
use serde::{Serialize, de::DeserializeOwned};
use serde_json::Value;

use crate::{BrpError, BrpResult, CommandTypeInfo, RemoteMethods};

/// Remote command handling module.
pub struct RpcCommand<T, Y: RemoteCommand<T>> {
    /// command input
    pub input: Option<Y::ParameterType>,
}

impl<T, Y: RemoteCommand<T>> Default for RpcCommand<T, Y> {
    fn default() -> Self {
        Self { input: None }
    }
}

impl<T, Y: RemoteCommand<T>> RpcCommand<T, Y> {
    /// Set the input for the RPC command.
    pub fn with_input(input: Y::ParameterType) -> Self {
        Self { input: Some(input) }
    }
}

impl<T: 'static + Send, Y: RemoteCommand<T>> Command<Result<Y::ResponseType, BrpError>>
    for RpcCommand<T, Y>
{
    fn apply(self, world: &mut World) -> Result<Y::ResponseType, BrpError> {
        Y::method_impl(self.input, world)
    }
}

/// Parses the input parameters for the command.
fn parse_input<T: Serialize + DeserializeOwned>(
    params: Option<Value>,
) -> Result<Option<T>, BrpError> {
    let command_input = match params {
        Some(json_value) => {
            match serde_json::from_value::<T>(json_value).map_err(BrpError::invalid_input) {
                Ok(v) => Some(v),
                Err(e) => return Err(e),
            }
        }
        None => None,
    };
    Ok(command_input)
}

/// Helper trait for creating RPC commands.
pub trait RemoteCommand<Output = Value>: bevy_reflect::GetTypeRegistration + Sized {
    /// Type of the input parameter for the command.
    type ParameterType: Serialize
        + DeserializeOwned
        + bevy_reflect::GetTypeRegistration
        + Send
        + 'static;
    /// Type of the response for the command.
    type ResponseType: Serialize
        + DeserializeOwned
        + bevy_reflect::GetTypeRegistration
        + Send
        + 'static;
    /// Path of the command.
    const RPC_PATH: &str;

    /// Returns the input parameter for the command.
    fn input_or_err(input: Option<Self::ParameterType>) -> Result<Self::ParameterType, BrpError> {
        input.ok_or(BrpError::missing_input())
    }

    /// Implementation of the command method that processes input and returns a response.
    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, BrpError>;

    /// Returns the type information for the command.
    fn get_command_type_info() -> CommandTypeInfo {
        CommandTypeInfo {
            command_type: Self::get_type_registration().type_id(),
            arg_type: TypeId::of::<Self::ParameterType>(),
            response_type: TypeId::of::<Self::ResponseType>(),
        }
    }
}

// /// Trait for remote commands that execute instantly and return a response.
// pub trait RemoteCommandInstant: RemoteCommand {
//     /// Returns the method handler for this instant remote command.
//     fn get_method_handler() -> RemoteMethodHandler {
//         RemoteMethodHandler::Instant(
//             Box::new(IntoSystem::into_system(command_system::<Self>)),
//             Some(get_command_type_info::<Self>()),
//         )
//     }
// }
pub(crate) fn command_system<T: RemoteCommand<Value>>(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult<Value> {
    let command_input = parse_input::<T::ParameterType>(params)?;
    let result = T::method_impl(command_input, world)?;

    serde_json::to_value(result).map_err(BrpError::internal)
}
pub(crate) fn watching_command_system<T: RemoteCommand<Option<Value>>>(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult<Option<Value>> {
    let command_input = parse_input::<T::ParameterType>(params)?;
    let result = T::method_impl(command_input, world)?;

    let value = serde_json::to_value(result).map_err(BrpError::internal)?;
    Ok(Some(value))
}
fn add_remote_command<T: RemoteCommand<Value>>(
    mut methods: ResMut<RemoteMethods>,
    mut commands: Commands,
) {
    let system_id = commands.register_system(command_system::<T>);
    methods.add_method::<T>(system_id);
}

fn add_remote_watching_command<T: RemoteCommand<Option<Value>>>(
    mut methods: ResMut<RemoteMethods>,
    mut commands: Commands,
) {
    let system_id = commands.register_system(watching_command_system::<T>);
    methods.add_watching_method::<T>(system_id);
}
/// Extension trait for adding remote command methods to the Bevy App.
pub trait RemoteCommandAppExt {
    /// Registers a remote method.
    fn add_remote_method<T: RemoteCommand>(&mut self) -> &mut Self;
    /// Registers a remote method that can return multiple values.
    fn add_remote_watching_method<T: RemoteCommand<Option<Value>>>(&mut self) -> &mut Self;
    /// Registers the types associated with a remote command for reflection.
    fn register_method_types<Y, T: RemoteCommand<Y>>(&mut self) -> &mut Self;

    /// Registers a remote method that can return value once.
    fn register_untyped_method<M>(
        &mut self,
        name: impl Into<String>,
        handler: impl IntoSystem<In<Option<Value>>, BrpResult, M>,
    ) -> &mut Self;
    /// Registers a remote method that can return values multiple times.
    fn register_untyped_watching_method<M>(
        &mut self,
        name: impl Into<String>,
        handler: impl IntoSystem<In<Option<Value>>, BrpResult<Option<Value>>, M>,
    ) -> &mut Self;
}

impl RemoteCommandAppExt for App {
    fn add_remote_method<T: RemoteCommand>(&mut self) -> &mut Self {
        self.register_method_types::<Value, T>()
            .add_systems(PreStartup, add_remote_command::<T>)
    }
    fn add_remote_watching_method<T: RemoteCommand<Option<Value>>>(&mut self) -> &mut Self {
        self.register_method_types::<Option<Value>, T>()
            .add_systems(PreStartup, add_remote_watching_command::<T>)
    }

    fn register_method_types<Y, T: RemoteCommand<Y>>(&mut self) -> &mut Self {
        self.register_type::<T>()
            .register_type::<T::ParameterType>()
            .register_type::<T::ResponseType>()
    }

    fn register_untyped_method<M>(
        &mut self,
        name: impl Into<String>,
        handler: impl IntoSystem<In<Option<Value>>, BrpResult, M>,
    ) -> &mut Self {
        let remote_handler = crate::RemoteMethodSystemId::Instant(
            self.main_mut()
                .world_mut()
                .register_boxed_system(Box::new(IntoSystem::into_system(handler))),
            None,
        );
        let name = name.into();
        self.main_mut()
            .world_mut()
            .get_resource_mut::<RemoteMethods>()
            .unwrap()
            .insert(name, remote_handler);
        self
    }

    fn register_untyped_watching_method<M>(
        &mut self,
        name: impl Into<String>,
        handler: impl IntoSystem<In<Option<Value>>, BrpResult<Option<Value>>, M>,
    ) -> &mut Self {
        let remote_handler = crate::RemoteMethodSystemId::Watching(
            self.main_mut()
                .world_mut()
                .register_boxed_system(Box::new(IntoSystem::into_system(handler))),
            None,
        );
        let name = name.into();
        self.main_mut()
            .world_mut()
            .get_resource_mut::<RemoteMethods>()
            .unwrap()
            .insert(name, remote_handler);
        self
    }
}

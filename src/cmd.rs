use bevy_app::{App, PreStartup};
use bevy_ecs::{
    system::{Command, Commands, In, ResMut},
    world::World,
};
use serde::{Serialize, de::DeserializeOwned};
use serde_json::Value;

use crate::{BrpError, BrpResult, RemoteMethods};

pub struct RpcCommand {
    pub path: String,
    pub input: Option<Value>,
}

impl RpcCommand {
    pub fn new(path: impl Into<String>) -> RpcCommand {
        RpcCommand {
            path: path.into(),
            input: None,
        }
    }

    pub fn with_input(&mut self, input: Value) -> &mut Self {
        self.input = Some(input);
        self
    }
}

impl Command for RpcCommand {
    fn apply(self, world: &mut World) {
        let Some(remote_id) = world
            .get_resource::<crate::RemoteMethods>()
            .and_then(|e| e.get(&self.path))
        else {
            return;
        };
        match remote_id {
            crate::RemoteMethodSystemId::Instant(system_id, ..) => {
                let output = world.run_system_with(*system_id, self.input);
                if let Ok(Ok(value)) = output {
                    bevy_log::info!("{}", serde_json::to_string_pretty(&value).expect(""));
                }
            }
            crate::RemoteMethodSystemId::Watching(system_id) => {
                let _ = world.run_system_with(*system_id, self.input);
            }
        }
    }
}

pub trait RemoteCommandSupport: bevy_reflect::GetTypeRegistration {
    type ParameterType: Serialize + DeserializeOwned + bevy_reflect::GetTypeRegistration;
    type ResponseType: Serialize + DeserializeOwned + bevy_reflect::GetTypeRegistration;
    const RPC_PATH: &str;

    fn input_or_err(input: Option<Self::ParameterType>) -> Result<Self::ParameterType, BrpError> {
        input.ok_or(BrpError::missing_input())
    }

    fn to_command(input: Option<Self::ParameterType>) -> RpcCommand {
        RpcCommand {
            path: Self::RPC_PATH.into(),
            input: serde_json::to_value(input).ok(),
        }
    }

    fn no_input() -> RpcCommand {
        RpcCommand {
            path: Self::RPC_PATH.into(),
            input: None,
        }
    }

    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, crate::BrpError>;
}

pub fn remote_command_system<T: RemoteCommandSupport>(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let command_input = match params {
        Some(json_value) => {
            match serde_json::from_value::<T::ParameterType>(json_value)
                .map_err(BrpError::invalid_input)
            {
                Ok(v) => Some(v),
                Err(e) => return Err(e),
            }
        }
        None => None,
    };

    match T::method_impl(command_input, world) {
        Ok(v) => match serde_json::to_value(v) {
            Ok(value) => Ok(value),
            Err(e) => Err(BrpError::internal(e)),
        },
        Err(e) => Err(e),
    }
}

/// Helper system that will schedule the command.
/// It will run the command with empty input.
pub fn run_with_no_input<T: RemoteCommandSupport>(mut commands: Commands) {
    commands.queue(T::no_input());
}

pub fn add_remote_command<T: RemoteCommandSupport>(
    mut methods: ResMut<RemoteMethods>,
    mut commands: Commands,
) {
    let system_id = commands.register_system(remote_command_system::<T>);
    methods.add_method::<T>(system_id);
}

pub trait RemoteCommandAppExt {
    fn add_remote_method<T: RemoteCommandSupport>(&mut self) -> &mut Self;
}

impl RemoteCommandAppExt for App {
    fn add_remote_method<T: RemoteCommandSupport>(&mut self) -> &mut Self {
        self.register_type::<T>()
            .register_type::<T::ParameterType>()
            .register_type::<T::ResponseType>()
            .add_systems(PreStartup, add_remote_command::<T>)
    }
}

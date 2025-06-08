//! Module with trimmed down `OpenRPC` document structs.
//! It tries to follow this standard: <https://spec.open-rpc.org>
use std::{any::Any, collections::HashSet};

use crate::RemoteMethods;

use super::json_schema::{
    json_schema::{
        BasicTypeInfoBuilder, JsonSchemaBasic, ReferenceLocation, ReflectJsonSchema,
        SchemaDefinitionsHelper, TypeReferenceId, TypeReferencePath,
    },
    reflect_helper::ReflectDocReader,
};
use bevy_platform::collections::HashMap;
use bevy_reflect::{FromType, Reflect, TypeRegistry};
use bevy_utils::default;
use serde::{Deserialize, Serialize};

/// Represents an `OpenRPC` document as defined by the `OpenRPC` specification.
#[derive(Debug, Serialize, Deserialize, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct OpenRpcDocument {
    /// The version of the `OpenRPC` specification being used.
    pub openrpc: String,
    /// Informational metadata about the document.
    pub info: InfoObject,
    /// List of RPC methods defined in the document.
    pub methods: Vec<MethodObject>,
    /// Optional list of server objects that provide the API endpoint details.
    pub servers: Option<Vec<ServerObject>>,
    #[reflect(ignore)]
    pub components: HashMap<TypeReferenceId, Box<JsonSchemaBasic>>,
}

impl FromType<OpenRpcDocument> for ReflectJsonSchema {
    fn from_type() -> Self {
        JsonSchemaBasic {
            ref_type: Some(TypeReferencePath::new_ref(
                ReferenceLocation::Url,
                "https://raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json",
            )),
            description: Some(
                "Represents an `OpenRPC` document as defined by the `OpenRPC` specification."
                    .to_string(),
            ),
            ..default()
        }
        .into()
    }
}

/// Contains metadata information about the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InfoObject {
    /// The title of the API or document.
    pub title: String,
    /// The version of the API.
    pub version: String,
    /// An optional description providing additional details about the API.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// A collection of custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<String, serde_json::Value>,
}

impl Default for InfoObject {
    fn default() -> Self {
        Self {
            title: "Bevy Remote Protocol".to_owned(),
            version: env!("CARGO_PKG_VERSION").to_owned(),
            description: None,
            extensions: Default::default(),
        }
    }
}

/// Describes a server hosting the API as specified in the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Default, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ServerObject {
    /// The name of the server.
    pub name: String,
    /// The URL endpoint of the server.
    pub url: String,
    /// An optional description of the server.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Additional custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<String, serde_json::Value>,
}

/// Represents an RPC method in the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Default, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct MethodObject {
    /// The method name (e.g., "/bevy/get")
    pub name: String,
    /// An optional short summary of the method.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub summary: Option<String>,
    /// An optional detailed description of the method.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub description: Option<String>,
    /// Parameters for the RPC method
    #[serde(default)]
    pub params: Vec<JsonSchemaBasic>,
    // /// The expected result of the method
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<JsonSchemaBasic>,
    /// Additional custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<String, serde_json::Value>,
}

impl MethodObject {
    pub fn get_referenced_types(&self) -> bevy_platform::collections::HashSet<TypeReferencePath> {
        let mut types = bevy_platform::collections::HashSet::new();
        for param in self.params.iter() {
            types.extend(param.get_referenced_types());
        }
        if let Some(result) = &self.result {
            types.extend(result.get_referenced_types());
        }
        types
    }
}

impl SchemaDefinitionsHelper for OpenRpcDocument {
    fn get_definitions(&self) -> &HashMap<TypeReferenceId, Box<JsonSchemaBasic>> {
        &self.components
    }

    fn get_referenced_types(&self) -> bevy_platform::collections::HashSet<TypeReferencePath> {
        let mut types = bevy_platform::collections::HashSet::new();
        for method in self.methods.iter() {
            types.extend(method.get_referenced_types());
        }
        for component in self.components.values() {
            types.extend(component.get_referenced_types());
        }
        types
    }

    fn add_definitions(&mut self, definitions: HashMap<TypeReferenceId, Box<JsonSchemaBasic>>) {
        self.components.extend(definitions);
    }
}

pub trait OpenRpcBuilder {
    fn build_open_rpc_schema(
        &self,
        methods: &RemoteMethods,
        servers: Option<Vec<ServerObject>>,
    ) -> OpenRpcDocument;
}

impl OpenRpcBuilder for &TypeRegistry {
    fn build_open_rpc_schema(
        &self,
        methods: &RemoteMethods,
        servers: Option<Vec<ServerObject>>,
    ) -> OpenRpcDocument {
        let empty_type_id = ().type_id();
        let mut referenced_types = HashSet::new();
        let methods: Vec<MethodObject> = methods
            .0
            .iter()
            .map(|(path, id)| {
                let mut method = MethodObject {
                    name: path.clone(),
                    ..default()
                };
                if let Some(typed_info) = id.remote_type_info() {
                    if let Some(mut schema) = self
                        .get(typed_info.arg_type)
                        .map(|r| self.build_json_schema_from_reg(r))
                    {
                        if typed_info.arg_type != empty_type_id {
                            if schema.ref_type.as_ref().is_some_and(|f| f.is_local()) {
                                schema.change_referenced_types_location(
                                    super::json_schema::json_schema::ReferenceLocation::Components,
                                );
                                referenced_types.extend(schema.get_referenced_types());
                            }
                            method.params.push(schema);
                        }
                    };
                    if let Some(description) = self
                        .get(typed_info.command_type)
                        .and_then(|r| r.type_info().to_description())
                    {
                        method.description = description.into();
                    };

                    if typed_info.response_type != empty_type_id {
                        if let Some(mut schema) = self
                            .get(typed_info.response_type)
                            .map(|r| self.build_json_schema_from_reg(r))
                        {
                            schema.change_referenced_types_location(
                                super::json_schema::json_schema::ReferenceLocation::Components,
                            );
                            referenced_types.extend(schema.get_referenced_types());
                            method.result = Some(schema);
                        };
                    }
                }

                method
            })
            .collect();

        let document = OpenRpcDocument {
            info: Default::default(),
            methods,
            openrpc: "1.3.2".to_owned(),
            servers,
            components: Default::default(),
        };
        self.build_with_definitions(&document)
    }
}

#[cfg(test)]
mod tests {
    use bevy_ecs::prelude::*;

    use crate::{
        builtin_methods::RpcDiscoverCommand,
        cmd::remote_command_system,
        schemas::{
            json_schema::json_schema::ReflectJsonSchema,
            open_rpc::{OpenRpcBuilder, OpenRpcDocument},
        },
    };

    #[test]
    pub fn test_open_rpc_document() {
        let mut world = World::default();
        world.init_resource::<AppTypeRegistry>();
        let mut methods = crate::RemoteMethods::new();
        {
            let id = world.register_system(remote_command_system::<RpcDiscoverCommand>);
            methods.add_method::<RpcDiscoverCommand>(id);

            let register = world.get_resource_mut::<AppTypeRegistry>().unwrap();
            let mut types = register.write();
            types.register::<RpcDiscoverCommand>();
            types.register::<OpenRpcDocument>();
            types.register_type_data::<OpenRpcDocument, ReflectJsonSchema>();
        }

        let servers = None;

        let type_registry = world.get_resource::<AppTypeRegistry>().unwrap().read();
        let document = (&*type_registry).build_open_rpc_schema(&methods, servers);

        eprintln!(
            "SCHEMA: {}",
            serde_json::to_string_pretty(&document).unwrap_or_default()
        );

        assert!(!document.methods.is_empty());
        assert!(document.components.is_empty());
        assert_eq!(document.openrpc, "1.3.2");
    }
}

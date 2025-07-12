//! Module with trimmed down `OpenRPC` document structs.
//! It tries to follow this standard: <https://spec.open-rpc.org>

use crate::schemas::reflect_info::{ReferenceLocation, TypeReferencePath};
use alloc::borrow::Cow;
use bevy_derive::{Deref, DerefMut};
use bevy_ecs::prelude::ReflectResource;
use bevy_ecs::resource::Resource;
use bevy_platform::collections::HashMap;
use bevy_reflect::{Reflect, ReflectDeserialize, ReflectSerialize};
use serde::{Deserialize, Serialize};

use super::json_schema::JsonSchemaBevyType;

/// Constant representing the version of the `OpenRPC` specification being used.
pub const OPENRPC_VERSION: &str = "1.3.2";

/// Represents an `OpenRPC` document as defined by the `OpenRPC` specification.
#[derive(Debug, Serialize, Deserialize, Reflect)]
#[serde(rename_all = "camelCase")]
pub struct OpenRpcDocument {
    /// The version of the `OpenRPC` specification being used.
    pub openrpc: Cow<'static, str>,
    /// Informational metadata about the document.
    pub info: InfoObject,
    /// List of RPC methods defined in the document.
    pub methods: Vec<MethodObject>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    /// List of server objects that provide the API endpoint details.
    pub servers: Vec<ServerObject>,
}

impl super::ExternalSchemaSource for OpenRpcDocument {
    fn get_external_schema_source() -> TypeReferencePath {
        TypeReferencePath::new_ref(
            ReferenceLocation::Url,
            "raw.githubusercontent.com/open-rpc/meta-schema/master/schema.json",
        )
    }
}

/// Resource containing information about the `OpenRPC` document.
/// When the resource is present in the game, it provides metadata about the document.
#[derive(Serialize, Deserialize, Debug, Reflect, Resource, Clone, Deref, DerefMut)]
#[reflect(Resource, Serialize, Deserialize, Clone)]
pub struct InfoObjectResource(InfoObject);

/// Contains metadata information about the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct InfoObject {
    /// The title of the API or document.
    pub title: Cow<'static, str>,
    /// The version of the API.
    pub version: Cow<'static, str>,
    /// An optional description providing additional details about the API.
    #[serde(skip_serializing_if = "str::is_empty", default)]
    pub description: Cow<'static, str>,
    /// A collection of custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<Cow<'static, str>, serde_json::Value>,
}

impl Default for InfoObject {
    fn default() -> Self {
        Self {
            title: "Bevy Remote Protocol".into(),
            version: env!("CARGO_PKG_VERSION").into(),
            description: "".into(),
            extensions: Default::default(),
        }
    }
}

/// Describes a server hosting the API as specified in the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Default, Reflect, Clone)]
#[serde(rename_all = "camelCase")]
pub struct ServerObject {
    /// The name of the server.
    pub name: Cow<'static, str>,
    /// The URL endpoint of the server.
    pub url: Cow<'static, str>,
    /// An optional description of the server.
    #[serde(skip_serializing_if = "str::is_empty")]
    pub description: Cow<'static, str>,
    /// Additional custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<Cow<'static, str>, serde_json::Value>,
}

/// Represents an RPC method in the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Default, Reflect)]
#[serde(rename_all = "camelCase")]
pub struct MethodObject {
    /// The method name (e.g., "/bevy/get")
    pub name: Cow<'static, str>,
    /// An optional short summary of the method.
    #[serde(skip_serializing_if = "str::is_empty", default)]
    pub summary: Cow<'static, str>,
    /// An optional detailed description of the method.
    #[serde(skip_serializing_if = "str::is_empty")]
    pub description: Cow<'static, str>,
    /// Parameters for the RPC method
    #[serde(default)]
    pub params: Vec<Parameter>,
    /// The expected result of the method
    #[serde(skip_serializing_if = "Option::is_none")]
    pub result: Option<Parameter>,
    /// Additional custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<Cow<'static, str>, serde_json::Value>,
}

/// Represents an RPC method parameter in the `OpenRPC` document.
#[derive(Serialize, Deserialize, Debug, Reflect, Default)]
#[serde(rename_all = "camelCase")]
pub struct Parameter {
    /// Parameter name
    pub name: Cow<'static, str>,
    /// Parameter description
    #[serde(skip_serializing_if = "str::is_empty", default)]
    pub description: Cow<'static, str>,
    /// An optional short summary of the parameter.
    #[serde(skip_serializing_if = "str::is_empty", default)]
    pub summary: Cow<'static, str>,
    /// JSON schema describing the parameter
    pub schema: JsonSchemaBevyType,
    /// Additional custom extension fields.
    #[serde(flatten)]
    #[reflect(ignore)]
    pub extensions: HashMap<Cow<'static, str>, serde_json::Value>,
}

impl From<JsonSchemaBevyType> for Parameter {
    fn from(schema: JsonSchemaBevyType) -> Self {
        Parameter {
            name: "parameter".into(),
            summary: schema.short_path.clone(),
            description: schema.description.clone().unwrap_or_default(),
            schema,
            ..Default::default()
        }
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_getting_schema() {}
}

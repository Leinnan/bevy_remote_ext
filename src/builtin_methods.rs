//! Built-in verbs for the Bevy Remote Protocol.

use alloc::borrow::Cow;
use core::any::TypeId;
use std::ops::Deref;

use anyhow::{Result as AnyhowResult, anyhow};
use bevy_derive::{Deref, DerefMut};
use bevy_ecs::{
    component::ComponentId,
    entity::Entity,
    event::EventCursor,
    hierarchy::ChildOf,
    query::QueryBuilder,
    reflect::{AppTypeRegistry, ReflectComponent, ReflectResource},
    removal_detection::RemovedComponentEntity,
    system::{In, Local},
    world::{EntityRef, EntityWorldMut, FilteredEntityRef, World},
};
use bevy_platform::collections::HashMap;
use bevy_reflect::{
    GetPath, PartialReflect, Reflect, TypeRegistration, TypeRegistry,
    serde::{ReflectSerializer, TypedReflectDeserializer},
};
use serde::{Deserialize, Serialize, de::DeserializeSeed as _};
use serde_json::{Map, Value};

use crate::{
    BrpError, BrpResult,
    cmd::RemoteCommand,
    error_codes,
    schemas::{
        json_schema::{JsonSchemaBevyType, JsonSchemaVariant, SchemaMarker},
        open_rpc::{InfoObject, MethodObject, OpenRpcDocument, Parameter, ServerObject},
        reflect_info::{TypeDefinitionBuilder, TypeReferencePath},
    },
};

/// The method path for a `bevy/get` request.
pub const BRP_GET_METHOD: &str = "bevy/get";

/// The method path for a `bevy/query` request.
pub const BRP_QUERY_METHOD: &str = "bevy/query";

/// The method path for a `bevy/spawn` request.
pub const BRP_SPAWN_METHOD: &str = "bevy/spawn";

/// The method path for a `bevy/insert` request.
pub const BRP_INSERT_METHOD: &str = "bevy/insert";

/// The method path for a `bevy/remove` request.
pub const BRP_REMOVE_METHOD: &str = "bevy/remove";

/// The method path for a `bevy/destroy` request.
pub const BRP_DESTROY_METHOD: &str = "bevy/destroy";

/// The method path for a `bevy/reparent` request.
pub const BRP_REPARENT_METHOD: &str = "bevy/reparent";

/// The method path for a `bevy/list` request.
pub const BRP_LIST_METHOD: &str = "bevy/list";

/// The method path for a `bevy/mutate_component` request.
pub const BRP_MUTATE_COMPONENT_METHOD: &str = "bevy/mutate_component";

/// The method path for a `bevy/get+watch` request.
pub const BRP_GET_AND_WATCH_METHOD: &str = "bevy/get+watch";

/// The method path for a `bevy/list+watch` request.
pub const BRP_LIST_AND_WATCH_METHOD: &str = "bevy/list+watch";

/// The method path for a `bevy/get_resource` request.
pub const BRP_GET_RESOURCE_METHOD: &str = "bevy/get_resource";

/// The method path for a `bevy/insert_resource` request.
pub const BRP_INSERT_RESOURCE_METHOD: &str = "bevy/insert_resource";

/// The method path for a `bevy/remove_resource` request.
pub const BRP_REMOVE_RESOURCE_METHOD: &str = "bevy/remove_resource";

/// The method path for a `bevy/mutate_resource` request.
pub const BRP_MUTATE_RESOURCE_METHOD: &str = "bevy/mutate_resource";

/// The method path for a `bevy/list_resources` request.
pub const BRP_LIST_RESOURCES_METHOD: &str = "bevy/list_resources";

/// The method path for a `bevy/registry/schema` request.
pub const BRP_REGISTRY_SCHEMA_METHOD: &str = "bevy/registry/schema";

/// The method path for a `rpc.discover` request.
pub const RPC_DISCOVER_METHOD: &str = "rpc.discover";

/// `bevy/get`: Retrieves one or more components from the entity with the given
/// ID.
///
/// The server responds with a [`BrpGetResponse`].
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpGetParams {
    /// The ID of the entity from which components are to be requested.
    pub entity: Entity,

    /// The [full paths] of the component types that are to be requested
    /// from the entity.
    ///
    /// Note that these strings must consist of the *full* type paths: e.g.
    /// `bevy_transform::components::transform::Transform`, not just
    /// `Transform`.
    ///
    /// [full paths]: bevy_reflect::TypePath::type_path
    pub components: Vec<String>,

    /// An optional flag to fail when encountering an invalid component rather
    /// than skipping it. Defaults to false.
    #[serde(default)]
    pub strict: bool,
}

/// `bevy/get_resource`: Retrieves the value of a given resource.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpGetResourceParams {
    /// The [full path] of the resource type being requested.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    pub resource: String,
}

/// `bevy/query`: Performs a query over components in the ECS, returning entities
/// and component values that match.
///
/// The server responds with a [`BrpQueryResponse`].
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpQueryParams {
    /// The components to select.
    pub data: BrpQuery,

    /// An optional filter that specifies which entities to include or
    /// exclude from the results.
    #[serde(default)]
    pub filter: BrpQueryFilter,

    /// An optional flag to fail when encountering an invalid component rather
    /// than skipping it. Defaults to false.
    #[serde(default)]
    pub strict: bool,
}

/// `bevy/spawn`: Creates a new entity with the given components and responds
/// with its ID.
///
/// The server responds with a [`BrpSpawnResponse`].
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpSpawnParams {
    /// A map from each component's full path to its serialized value.
    ///
    /// These components will be added to the entity.
    ///
    /// Note that the keys of the map must be the [full type paths]: e.g.
    /// `bevy_transform::components::transform::Transform`, not just
    /// `Transform`.
    ///
    /// [full type paths]: bevy_reflect::TypePath::type_path
    pub components: HashMap<String, BrpValue>,
}

/// `bevy/destroy`: Given an ID, despawns the entity with that ID.
///
/// The server responds with an okay.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct BrpDestroyParams {
    /// The ID of the entity to despawn.
    pub entity: Entity,
}

/// `bevy/remove`: Deletes one or more components from an entity.
///
/// The server responds with a null.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpRemoveParams {
    /// The ID of the entity from which components are to be removed.
    pub entity: Entity,

    /// The full paths of the component types that are to be removed from
    /// the entity.
    ///
    /// Note that these strings must consist of the [full type paths]: e.g.
    /// `bevy_transform::components::transform::Transform`, not just
    /// `Transform`.
    ///
    /// [full type paths]: bevy_reflect::TypePath::type_path
    pub components: Vec<String>,
}

/// `bevy/remove_resource`: Removes the given resource from the world.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpRemoveResourceParams {
    /// The [full path] of the resource type to remove.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    pub resource: String,
}

/// `bevy/insert`: Adds one or more components to an entity.
///
/// The server responds with a null.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpInsertParams {
    /// The ID of the entity that components are to be added to.
    pub entity: Entity,

    /// A map from each component's full path to its serialized value.
    ///
    /// These components will be added to the entity.
    ///
    /// Note that the keys of the map must be the [full type paths]: e.g.
    /// `bevy_transform::components::transform::Transform`, not just
    /// `Transform`.
    ///
    /// [full type paths]: bevy_reflect::TypePath::type_path
    pub components: HashMap<String, BrpValue>,
}

/// `bevy/insert_resource`: Inserts a resource into the world with a given
/// value.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpInsertResourceParams {
    /// The [full path] of the resource type to insert.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    pub resource: String,

    /// The serialized value of the resource to be inserted.
    pub value: BrpValue,
}

/// `bevy/reparent`: Assign a new parent to one or more entities.
///
/// The server responds with a null.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct BrpReparentParams {
    /// The IDs of the entities that are to become the new children of the
    /// `parent`.
    pub entities: Vec<Entity>,

    /// The IDs of the entity that will become the new parent of the
    /// `entities`.
    ///
    /// If this is `None`, then the entities are removed from all parents.
    #[serde(default)]
    pub parent: Option<Entity>,
}

/// `bevy/list`: Returns a list of all type names of registered components in the
/// system (no params provided), or those on an entity (params provided).
///
/// The server responds with a [`BrpListResponse`]
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpListParams {
    /// The entity to query.
    pub entity: Entity,
}

/// `bevy/mutate_component`:
///
/// The server responds with a null.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpMutateComponentParams {
    /// The entity of the component to mutate.
    pub entity: Entity,

    /// The [full path] of the component to mutate.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    pub component: String,

    /// The [path] of the field within the component.
    ///
    /// [path]: bevy_reflect::GetPath
    pub path: String,

    /// The value to insert at `path`.
    pub value: BrpValue,
}

/// Dummy struct that holds `serde_json::Value`. Used to have any type info in BRP structs.
#[derive(Reflect, Debug, DerefMut, Deref, Clone, Eq, PartialEq, Hash, Serialize, Deserialize)]
pub struct BrpValue(#[reflect(ignore)] pub Value);

/// `bevy/mutate_resource`:
///
/// The server responds with a null.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct BrpMutateResourceParams {
    /// The [full path] of the resource to mutate.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    pub resource: String,

    /// The [path] of the field within the resource.
    ///
    /// [path]: bevy_reflect::GetPath
    pub path: String,

    /// The value to insert at `path`.
    pub value: Value,
}

/// Describes the data that is to be fetched in a query.
#[derive(Debug, Serialize, Deserialize, Clone, Default, PartialEq, Reflect)]
pub struct BrpQuery {
    /// The [full path] of the type name of each component that is to be
    /// fetched.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    #[serde(default)]
    pub components: Vec<String>,

    /// The [full path] of the type name of each component that is to be
    /// optionally fetched.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    #[serde(default)]
    pub option: Vec<String>,

    /// The [full path] of the type name of each component that is to be checked
    /// for presence.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    #[serde(default)]
    pub has: Vec<String>,
}

/// Additional constraints that can be placed on a query to include or exclude
/// certain entities.
#[derive(Debug, Serialize, Deserialize, Clone, Default, PartialEq, Reflect)]
pub struct BrpQueryFilter {
    /// The [full path] of the type name of each component that must not be
    /// present on the entity for it to be included in the results.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    #[serde(default)]
    pub without: Vec<String>,

    /// The [full path] of the type name of each component that must be present
    /// on the entity for it to be included in the results.
    ///
    /// [full path]: bevy_reflect::TypePath::type_path
    #[serde(default)]
    pub with: Vec<String>,
}

/// Constraints that can be placed on a query to include or exclude
/// certain definitions.
#[derive(Debug, Serialize, Deserialize, Clone, Default, PartialEq, Reflect)]
pub struct BrpJsonSchemaQueryFilter {
    /// The crate name of the type name of each component that must not be
    /// present on the entity for it to be included in the results.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub without_crates: Vec<String>,

    /// The crate name of the type name of each component that must be present
    /// on the entity for it to be included in the results.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub with_crates: Vec<String>,

    /// Constrain resource by type
    #[serde(default)]
    pub type_limit: JsonSchemaTypeLimit,

    /// Add `one_of` field to the root of the schema.
    /// It will allow to use the schema for validation values if they match the format used by the Bevy reflect serialization.
    #[serde(default)]
    pub meta_schemas_export: bool,
}

impl BrpJsonSchemaQueryFilter {
    /// Check if the filter should skip a type registration based on the crate name.
    pub fn should_skip_for_crate(&self, type_registration: &TypeRegistration) -> bool {
        let crate_name = type_registration
            .type_info()
            .type_path_table()
            .crate_name()
            .unwrap_or_default();
        if !self.with_crates.is_empty() && !self.with_crates.iter().any(|c| crate_name.eq(c)) {
            return true;
        }
        if !self.without_crates.is_empty() && self.without_crates.iter().any(|c| crate_name.eq(c)) {
            return true;
        }
        false
    }
}

/// Additional [`BrpJsonSchemaQueryFilter`] constraints that can be placed on a query to include or exclude
/// certain definitions.
#[derive(Debug, Serialize, Deserialize, Clone, Default, PartialEq, Reflect)]
pub struct JsonSchemaTypeLimit {
    /// Schema cannot have specified reflect types
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub without: Vec<String>,

    /// Schema needs to have specified reflect types
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub with: Vec<String>,
}

impl JsonSchemaTypeLimit {
    /// Check if the type limit is empty.
    pub fn is_empty(&self) -> bool {
        self.without.is_empty() && self.with.is_empty()
    }

    /// Check if the type limit should skip a type.
    pub fn should_skip_type(&self, registered_types: &[&Cow<'_, str>]) -> bool {
        if !self.with.is_empty()
            && !self
                .with
                .iter()
                .any(|c| registered_types.iter().any(|cc| c.eq(*cc)))
        {
            return true;
        }
        if !self.without.is_empty()
            && self
                .without
                .iter()
                .any(|c| registered_types.iter().any(|cc| c.eq(*cc)))
        {
            return true;
        }
        false
    }
}

/// A response from the world to the client that specifies a single entity.
///
/// This is sent in response to `bevy/spawn`.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpSpawnResponse {
    /// The ID of the entity in question.
    pub entity: Entity,
}

/// The response to a `bevy/get` request.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
#[serde(untagged)]
pub enum BrpGetResponse {
    /// The non-strict response that reports errors separately without failing the entire request.
    Lenient {
        #[reflect(ignore)]
        /// A map of successful components with their values.
        components: HashMap<String, Value>,
        #[reflect(ignore)]
        /// A map of unsuccessful components with their errors.
        errors: HashMap<String, Value>,
    },
    /// The strict response that will fail if any components are not present or aren't
    /// reflect-able.
    Strict(#[reflect(ignore)] HashMap<String, Value>),
}

/// The response to a `bevy/get_resource` request.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpGetResourceResponse {
    /// The value of the requested resource.
    pub value: BrpValue,
}

/// A single response from a `bevy/get+watch` request.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq)]
#[serde(untagged)]
pub enum BrpGetWatchingResponse {
    /// The non-strict response that reports errors separately without failing the entire request.
    Lenient {
        /// A map of successful components with their values that were added or changes in the last
        /// tick.
        components: HashMap<String, Value>,
        /// An array of components that were been removed in the last tick.
        removed: Vec<String>,
        /// A map of unsuccessful components with their errors.
        errors: HashMap<String, Value>,
    },
    /// The strict response that will fail if any components are not present or aren't
    /// reflect-able.
    Strict {
        /// A map of successful components with their values that were added or changes in the last
        /// tick.
        components: HashMap<String, Value>,
        /// An array of components that were been removed in the last tick.
        removed: Vec<String>,
    },
}

/// The response to a `bevy/list` request.
pub type BrpListResponse = Vec<String>;

/// The response to a `bevy/list_resources` request.
pub type BrpListResourcesResponse = Vec<String>;

/// A single response from a `bevy/list+watch` request.
#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq)]
pub struct BrpListWatchingResponse {
    added: Vec<String>,
    removed: Vec<String>,
}

/// The response to a `bevy/query` request.
#[derive(Debug, Default, Serialize, Deserialize, Clone, PartialEq, Reflect, Deref, DerefMut)]
pub struct BrpQueryResponse(pub Vec<BrpQueryRow>);

/// One query match result: a single entity paired with the requested components.
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
pub struct BrpQueryRow {
    /// The ID of the entity that matched.
    pub entity: Entity,

    /// The serialized values of the requested components.
    #[reflect(ignore)]
    pub components: HashMap<String, Value>,

    /// The boolean-only containment query results.
    #[reflect(ignore)]
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    pub has: HashMap<String, Value>,
}

/// A helper function used to parse a `serde_json::Value`.
fn parse<T: for<'de> Deserialize<'de>>(value: Value) -> Result<T, BrpError> {
    serde_json::from_value(value).map_err(BrpError::invalid_input)
}

/// A helper function used to parse a `serde_json::Value` wrapped in an `Option`.
fn parse_some<T: for<'de> Deserialize<'de>>(value: Option<Value>) -> Result<T, BrpError> {
    match value {
        Some(value) => parse(value),
        None => Err(BrpError::missing_input()),
    }
}

/// Retrieves one or more components from the entity with the given
/// ID.
#[derive(Reflect, Debug)]
pub struct BevyGetCommand;

impl RemoteCommand for BevyGetCommand {
    type ParameterType = BrpGetParams;

    type ResponseType = BrpGetResponse;

    const RPC_PATH: &str = BRP_GET_METHOD;

    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<BrpGetResponse, crate::BrpError> {
        let BrpGetParams {
            entity,
            components,
            strict,
        } = Self::input_or_err(input)?;
        let app_type_registry = world.resource::<AppTypeRegistry>();
        let type_registry = app_type_registry.read();
        let entity_ref = get_entity(world, entity)?;

        let response =
            reflect_components_to_response(components, strict, entity, entity_ref, &type_registry)?;
        Ok(response)
    }
}

/// Retrieves the value of a given resource.
#[derive(Reflect)]
pub struct BevyGetResourceCommand;
impl RemoteCommand for BevyGetResourceCommand {
    type ParameterType = BrpGetResourceParams;

    type ResponseType = BrpGetResourceResponse;

    const RPC_PATH: &str = BRP_GET_RESOURCE_METHOD;
    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<BrpGetResourceResponse, crate::BrpError> {
        let BrpGetResourceParams {
            resource: resource_path,
        } = Self::input_or_err(input)?;
        let type_registry = world.resource::<AppTypeRegistry>().read();
        let reflect_resource = get_reflect_resource(&type_registry, &resource_path)
            .map_err(BrpError::resource_error)?;

        let Ok(reflected) = reflect_resource.reflect(&*world) else {
            return Err(BrpError::resource_not_present(&resource_path));
        };

        // Use the `ReflectSerializer` to serialize the value of the resource;
        // this produces a map with a single item.
        let reflect_serializer =
            ReflectSerializer::new(reflected.as_partial_reflect(), &type_registry);
        let Value::Object(serialized_object) =
            serde_json::to_value(&reflect_serializer).map_err(BrpError::resource_error)?
        else {
            return Err(BrpError {
                code: error_codes::RESOURCE_ERROR,
                message: format!("Resource `{}` could not be serialized", resource_path),
                data: None,
            });
        };

        // Get the single value out of the map.
        let value = serialized_object.into_values().next().ok_or_else(|| {
            BrpError::internal(anyhow!("Unexpected format of serialized resource value"))
        })?;
        let response = BrpGetResourceResponse {
            value: BrpValue(value),
        };
        Ok(response)
    }
}

/// Handles a `bevy/get+watch` request coming from a client.
pub fn process_remote_get_watching_request(
    In(params): In<Option<Value>>,
    world: &World,
    mut removal_cursors: Local<HashMap<ComponentId, EventCursor<RemovedComponentEntity>>>,
) -> BrpResult<Option<Value>> {
    let BrpGetParams {
        entity,
        components,
        strict,
    } = parse_some(params)?;

    let app_type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = app_type_registry.read();
    let entity_ref = get_entity(world, entity)?;

    let mut changed = Vec::new();
    let mut removed = Vec::new();
    let mut errors = <HashMap<_, _>>::default();

    'component_loop: for component_path in components {
        let Ok(type_registration) =
            get_component_type_registration(&type_registry, &component_path)
        else {
            let err =
                BrpError::component_error(format!("Unknown component type: `{component_path}`"));
            if strict {
                return Err(err);
            }
            errors.insert(
                component_path,
                serde_json::to_value(err).map_err(BrpError::internal)?,
            );
            continue;
        };
        let Some(component_id) = world.components().get_id(type_registration.type_id()) else {
            let err = BrpError::component_error(format!("Unknown component: `{component_path}`"));
            if strict {
                return Err(err);
            }
            errors.insert(
                component_path,
                serde_json::to_value(err).map_err(BrpError::internal)?,
            );
            continue;
        };

        if let Some(ticks) = entity_ref.get_change_ticks_by_id(component_id) {
            if ticks.is_changed(world.last_change_tick(), world.read_change_tick()) {
                changed.push(component_path);
                continue;
            }
        };

        let Some(events) = world.removed_components().get(component_id) else {
            continue;
        };
        let cursor = removal_cursors
            .entry(component_id)
            .or_insert_with(|| events.get_cursor());
        for event in cursor.read(events) {
            if Entity::from(event.clone()) == entity {
                removed.push(component_path);
                continue 'component_loop;
            }
        }
    }

    if changed.is_empty() && removed.is_empty() {
        return Ok(None);
    }

    let response =
        reflect_components_to_response(changed, strict, entity, entity_ref, &type_registry)?;

    let response = match response {
        BrpGetResponse::Lenient {
            components,
            errors: mut errs,
        } => BrpGetWatchingResponse::Lenient {
            components,
            removed,
            errors: {
                errs.extend(errors);
                errs
            },
        },
        BrpGetResponse::Strict(components) => BrpGetWatchingResponse::Strict {
            components,
            removed,
        },
    };

    Ok(Some(
        serde_json::to_value(response).map_err(BrpError::internal)?,
    ))
}

/// Reflect a list of components on an entity into a [`BrpGetResponse`].
fn reflect_components_to_response(
    components: Vec<String>,
    strict: bool,
    entity: Entity,
    entity_ref: EntityRef,
    type_registry: &TypeRegistry,
) -> BrpResult<BrpGetResponse> {
    let mut response = if strict {
        BrpGetResponse::Strict(Default::default())
    } else {
        BrpGetResponse::Lenient {
            components: Default::default(),
            errors: Default::default(),
        }
    };

    for component_path in components {
        match reflect_component(&component_path, entity, entity_ref, type_registry) {
            Ok(serialized_object) => match response {
                BrpGetResponse::Strict(ref mut components)
                | BrpGetResponse::Lenient {
                    ref mut components, ..
                } => {
                    components.extend(serialized_object.into_iter());
                }
            },
            Err(err) => match response {
                BrpGetResponse::Strict(_) => return Err(err),
                BrpGetResponse::Lenient { ref mut errors, .. } => {
                    let err_value = serde_json::to_value(err).map_err(BrpError::internal)?;
                    errors.insert(component_path, err_value);
                }
            },
        }
    }

    Ok(response)
}

/// Reflect a single component on an entity with the given component path.
fn reflect_component(
    component_path: &str,
    entity: Entity,
    entity_ref: EntityRef,
    type_registry: &TypeRegistry,
) -> BrpResult<Map<String, Value>> {
    let reflect_component =
        get_reflect_component(type_registry, component_path).map_err(BrpError::component_error)?;

    // Retrieve the reflected value for the given specified component on the given entity.
    let Some(reflected) = reflect_component.reflect(entity_ref) else {
        return Err(BrpError::component_not_present(component_path, entity));
    };

    // Each component value serializes to a map with a single entry.
    let reflect_serializer = ReflectSerializer::new(reflected.as_partial_reflect(), type_registry);
    let Value::Object(serialized_object) =
        serde_json::to_value(&reflect_serializer).map_err(BrpError::component_error)?
    else {
        return Err(BrpError {
            code: error_codes::COMPONENT_ERROR,
            message: format!("Component `{}` could not be serialized", component_path),
            data: None,
        });
    };

    Ok(serialized_object)
}

/// Performs a query over components in the ECS, returning entities
/// and component values that match.
#[derive(Reflect, Debug)]
pub struct BevyQueryCommand;

impl RemoteCommand for BevyQueryCommand {
    type ParameterType = BrpQueryParams;

    type ResponseType = BrpQueryResponse;

    const RPC_PATH: &str = BRP_QUERY_METHOD;
    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, crate::BrpError> {
        let BrpQueryParams {
            data:
                BrpQuery {
                    components,
                    option,
                    has,
                },
            filter: BrpQueryFilter { without, with },
            strict,
        } = Self::input_or_err(input)?;

        let app_type_registry = world.resource::<AppTypeRegistry>().clone();
        let type_registry = app_type_registry.read();

        let (components, unregistered_in_components) =
            get_component_ids(&type_registry, world, components, strict)
                .map_err(BrpError::component_error)?;
        let (option, _) = get_component_ids(&type_registry, world, option, strict)
            .map_err(BrpError::component_error)?;
        let (has, unregistered_in_has) = get_component_ids(&type_registry, world, has, strict)
            .map_err(BrpError::component_error)?;
        let (without, _) = get_component_ids(&type_registry, world, without, strict)
            .map_err(BrpError::component_error)?;
        let (with, unregistered_in_with) = get_component_ids(&type_registry, world, with, strict)
            .map_err(BrpError::component_error)?;

        // When "strict" is false:
        // - Unregistered components in "option" and "without" are ignored.
        // - Unregistered components in "has" are considered absent from the entity.
        // - Unregistered components in "components" and "with" result in an empty
        // response since they specify hard requirements.
        if !unregistered_in_components.is_empty() || !unregistered_in_with.is_empty() {
            return Ok(BrpQueryResponse::default());
        }

        let mut query = QueryBuilder::<FilteredEntityRef>::new(world);
        for (_, component) in &components {
            query.ref_id(*component);
        }
        for (_, option) in &option {
            query.optional(|query| {
                query.ref_id(*option);
            });
        }
        for (_, has) in &has {
            query.optional(|query| {
                query.ref_id(*has);
            });
        }
        for (_, without) in without {
            query.without_id(without);
        }
        for (_, with) in with {
            query.with_id(with);
        }

        // At this point, we can safely unify `components` and `option`, since we only retrieved
        // entities that actually have all the `components` already.
        //
        // We also will just collect the `ReflectComponent` values from the type registry all
        // at once so that we can reuse them between components.
        let paths_and_reflect_components: Vec<(&str, &ReflectComponent)> = components
            .into_iter()
            .chain(option)
            .map(|(type_id, _)| reflect_component_from_id(type_id, &type_registry))
            .collect::<AnyhowResult<Vec<(&str, &ReflectComponent)>>>()
            .map_err(BrpError::component_error)?;

        // ... and the analogous construction for `has`:
        let has_paths_and_reflect_components: Vec<(&str, &ReflectComponent)> = has
            .into_iter()
            .map(|(type_id, _)| reflect_component_from_id(type_id, &type_registry))
            .collect::<AnyhowResult<Vec<(&str, &ReflectComponent)>>>()
            .map_err(BrpError::component_error)?;

        let mut response = BrpQueryResponse::default();
        let mut query = query.build();
        for row in query.iter(world) {
            // The map of component values:
            let components_map = build_components_map(
                row.clone(),
                paths_and_reflect_components.iter().copied(),
                &type_registry,
            )
            .map_err(BrpError::component_error)?;

            // The map of boolean-valued component presences:
            let has_map = build_has_map(
                row.clone(),
                has_paths_and_reflect_components.iter().copied(),
                &unregistered_in_has,
            );
            response.push(BrpQueryRow {
                entity: row.id(),
                components: components_map,
                has: has_map,
            });
        }
        Ok(response)
    }
}

/// Creates a new entity with the given components and responds
/// with its ID.
#[derive(Reflect)]
pub struct BevySpawnCommand;

impl RemoteCommand for BevySpawnCommand {
    type ParameterType = BrpSpawnParams;

    type ResponseType = BrpSpawnResponse;

    const RPC_PATH: &str = BRP_SPAWN_METHOD;
    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, crate::BrpError> {
        let BrpSpawnParams { components } = Self::input_or_err(input)?;

        let app_type_registry = world.resource::<AppTypeRegistry>().clone();
        let type_registry = app_type_registry.read();

        let reflect_components = deserialize_components(&type_registry, components)
            .map_err(BrpError::component_error)?;

        let entity = world.spawn_empty();
        let entity_id = entity.id();
        insert_reflected_components(&type_registry, entity, reflect_components)
            .map_err(BrpError::component_error)?;

        Ok(BrpSpawnResponse { entity: entity_id })
    }
}

/// Returns an OpenRPC schema as a description of this service
#[derive(Reflect)]
pub struct RpcDiscoverCommand;

impl RemoteCommand for RpcDiscoverCommand {
    type ParameterType = ();
    type ResponseType = OpenRpcDocument;

    const RPC_PATH: &str = RPC_DISCOVER_METHOD;
    fn method_impl(
        _input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, crate::BrpError> {
        let remote_methods = world
            .get_resource::<crate::RemoteMethods>()
            .ok_or(BrpError::resource_not_present("bevy_remote::RemoteMethods"))?;
        let rpc_info = match world.get_resource::<crate::schemas::open_rpc::InfoObjectResource>() {
            Some(s) => s.deref().clone(),
            None => InfoObject::default(),
        };

        let servers: Vec<ServerObject> = remote_methods
            .server_list()
            .into_iter()
            .map(|server| server.clone())
            .collect();
        let types = world.resource::<AppTypeRegistry>();
        let types = types.read();
        let extra_info = world.resource::<crate::schemas::SchemaTypesMetadata>();

        let methods = remote_methods
            .mappings
            .iter()
            .map(|(name, info)| {
                let Some(type_info) = info.remote_type_info() else {
                    return MethodObject {
                        name: name.clone(),
                        ..Default::default()
                    };
                };
                #[cfg(feature = "documentation")]
                let summary: Cow<'static, str> = types
                    .get(type_info.command_type)
                    .and_then(|t| t.type_info().docs().map(|s| s.into()))
                    .unwrap_or_default();
                #[cfg(not(feature = "documentation"))]
                let summary: Cow<'static, str> = "".into();
                let params = if type_info.arg_type.eq(&TypeId::of::<()>()) {
                    [].into()
                } else {
                    if let Some(schema) = types
                        .build_schema_for_type_id_with_definitions(type_info.arg_type, extra_info)
                    {
                        let parameter = Parameter {
                            name: "input".into(),
                            ..schema.into()
                        };
                        [parameter].into()
                    } else {
                        [].into()
                    }
                };
                let result = if type_info.response_type.eq(&TypeId::of::<()>()) {
                    None
                } else {
                    let result_schema = types.build_schema_for_type_id_with_definitions(
                        type_info.response_type,
                        extra_info,
                    );
                    result_schema.map(|schema| Parameter {
                        name: "input".into(),
                        ..schema.into()
                    })
                };
                MethodObject {
                    name: name.clone(),
                    summary,
                    params,
                    result,
                    ..Default::default()
                }
            })
            .collect();

        let doc = OpenRpcDocument {
            info: rpc_info,
            methods,
            servers,
            openrpc: crate::schemas::open_rpc::OPENRPC_VERSION.into(),
        };

        Ok(doc)
    }
}
/// Adds one or more components to an entity.
#[derive(Reflect, Debug)]
pub struct BevyInsertCommand;
impl RemoteCommand for BevyInsertCommand {
    type ParameterType = BrpInsertParams;

    type ResponseType = ();

    const RPC_PATH: &str = BRP_INSERT_METHOD;
    fn method_impl(
        input: Option<Self::ParameterType>,
        world: &mut World,
    ) -> Result<Self::ResponseType, crate::BrpError> {
        let BrpInsertParams { entity, components } = Self::input_or_err(input)?;

        let app_type_registry = world.resource::<AppTypeRegistry>().clone();
        let type_registry = app_type_registry.read();

        let reflect_components = deserialize_components(&type_registry, components)
            .map_err(BrpError::component_error)?;

        insert_reflected_components(
            &type_registry,
            get_entity_mut(world, entity)?,
            reflect_components,
        )
        .map_err(BrpError::component_error)?;
        Ok(())
    }
}

/// Handles a `bevy/insert_resource` request coming from a client.
pub fn process_remote_insert_resource_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpInsertResourceParams {
        resource: resource_path,
        value,
    } = parse_some(params)?;

    let app_type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = app_type_registry.read();

    let reflected_resource = deserialize_resource(&type_registry, &resource_path, value.0)
        .map_err(BrpError::resource_error)?;

    let reflect_resource =
        get_reflect_resource(&type_registry, &resource_path).map_err(BrpError::resource_error)?;
    reflect_resource.insert(world, &*reflected_resource, &type_registry);

    Ok(Value::Null)
}

/// Handles a `bevy/mutate_component` request coming from a client.
///
/// This method allows you to mutate a single field inside an Entity's
/// component.
pub fn process_remote_mutate_component_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpMutateComponentParams {
        entity,
        component,
        path,
        value,
    } = parse_some(params)?;
    let app_type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = app_type_registry.read();

    // Get the fully-qualified type names of the component to be mutated.
    let component_type: &TypeRegistration = type_registry
        .get_with_type_path(&component)
        .ok_or_else(|| {
            BrpError::component_error(anyhow!("Unknown component type: `{}`", component))
        })?;

    // Get the reflected representation of the component.
    let mut reflected = component_type
        .data::<ReflectComponent>()
        .ok_or_else(|| {
            BrpError::component_error(anyhow!("Component `{}` isn't registered", component))
        })?
        .reflect_mut(world.entity_mut(entity))
        .ok_or_else(|| {
            BrpError::component_error(anyhow!("Cannot reflect component `{}`", component))
        })?;

    // Get the type of the field in the component that is to be
    // mutated.
    let value_type: &TypeRegistration = type_registry
        .get_with_type_path(
            reflected
                .reflect_path(path.as_str())
                .map_err(BrpError::component_error)?
                .reflect_type_path(),
        )
        .ok_or_else(|| {
            BrpError::component_error(anyhow!("Unknown component field type: `{}`", component))
        })?;

    // Get the reflected representation of the value to be inserted
    // into the component.
    let value: Box<dyn PartialReflect> = TypedReflectDeserializer::new(value_type, &type_registry)
        .deserialize(&value.0)
        .map_err(BrpError::component_error)?;

    // Apply the mutation.
    reflected
        .reflect_path_mut(path.as_str())
        .map_err(BrpError::component_error)?
        .try_apply(value.as_ref())
        .map_err(BrpError::component_error)?;

    Ok(Value::Null)
}

/// Handles a `bevy/mutate_resource` request coming from a client.
pub fn process_remote_mutate_resource_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpMutateResourceParams {
        resource: resource_path,
        path: field_path,
        value,
    } = parse_some(params)?;

    let app_type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = app_type_registry.read();

    // Get the `ReflectResource` for the given resource path.
    let reflect_resource =
        get_reflect_resource(&type_registry, &resource_path).map_err(BrpError::resource_error)?;

    // Get the actual resource value from the world as a `dyn Reflect`.
    let mut reflected_resource = reflect_resource
        .reflect_mut(world)
        .map_err(|_| BrpError::resource_not_present(&resource_path))?;

    // Get the type registration for the field with the given path.
    let value_registration = type_registry
        .get_with_type_path(
            reflected_resource
                .reflect_path(field_path.as_str())
                .map_err(BrpError::resource_error)?
                .reflect_type_path(),
        )
        .ok_or_else(|| {
            BrpError::resource_error(anyhow!("Unknown resource field type: `{}`", resource_path))
        })?;

    // Use the field's type registration to deserialize the given value.
    let deserialized_value: Box<dyn PartialReflect> =
        TypedReflectDeserializer::new(value_registration, &type_registry)
            .deserialize(&value)
            .map_err(BrpError::resource_error)?;

    // Apply the value to the resource.
    reflected_resource
        .reflect_path_mut(field_path.as_str())
        .map_err(BrpError::resource_error)?
        .try_apply(&*deserialized_value)
        .map_err(BrpError::resource_error)?;

    Ok(Value::Null)
}

/// Handles a `bevy/remove` request (remove components) coming from a client.
pub fn process_remote_remove_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpRemoveParams { entity, components } = parse_some(params)?;

    let app_type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = app_type_registry.read();

    let component_ids = get_component_ids(&type_registry, world, components, true)
        .and_then(|(registered, unregistered)| {
            if unregistered.is_empty() {
                Ok(registered)
            } else {
                Err(anyhow!("Unregistered component types: {:?}", unregistered))
            }
        })
        .map_err(BrpError::component_error)?;

    // Remove the components.
    let mut entity_world_mut = get_entity_mut(world, entity)?;
    for (_, component_id) in component_ids.iter() {
        entity_world_mut.remove_by_id(*component_id);
    }

    Ok(Value::Null)
}

/// Handles a `bevy/remove_resource` request coming from a client.
pub fn process_remote_remove_resource_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpRemoveResourceParams {
        resource: resource_path,
    } = parse_some(params)?;

    let app_type_registry = world.resource::<AppTypeRegistry>().clone();
    let type_registry = app_type_registry.read();

    let reflect_resource =
        get_reflect_resource(&type_registry, &resource_path).map_err(BrpError::resource_error)?;
    reflect_resource.remove(world);

    Ok(Value::Null)
}

/// Handles a `bevy/destroy` (despawn entity) request coming from a client.
pub fn process_remote_destroy_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpDestroyParams { entity } = parse_some(params)?;

    get_entity_mut(world, entity)?.despawn();

    Ok(Value::Null)
}

/// Handles a `bevy/reparent` request coming from a client.
pub fn process_remote_reparent_request(
    In(params): In<Option<Value>>,
    world: &mut World,
) -> BrpResult {
    let BrpReparentParams {
        entities,
        parent: maybe_parent,
    } = parse_some(params)?;

    // If `Some`, reparent the entities.
    if let Some(parent) = maybe_parent {
        let mut parent_commands =
            get_entity_mut(world, parent).map_err(|_| BrpError::entity_not_found(parent))?;
        for entity in entities {
            if entity == parent {
                return Err(BrpError::self_reparent(entity));
            }
            parent_commands.add_child(entity);
        }
    }
    // If `None`, remove the entities' parents.
    else {
        for entity in entities {
            get_entity_mut(world, entity)?.remove::<ChildOf>();
        }
    }

    Ok(Value::Null)
}

/// Handles a `bevy/list` request (list all components) coming from a client.
pub fn process_remote_list_request(In(params): In<Option<Value>>, world: &World) -> BrpResult {
    let app_type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = app_type_registry.read();

    let mut response = BrpListResponse::default();

    // If `Some`, return all components of the provided entity.
    if let Some(BrpListParams { entity }) = params.map(parse).transpose()? {
        let entity = get_entity(world, entity)?;
        for component_id in entity.archetype().components() {
            let Some(component_info) = world.components().get_info(component_id) else {
                continue;
            };
            response.push(component_info.name().to_owned());
        }
    }
    // If `None`, list all registered components.
    else {
        for registered_type in type_registry.iter() {
            if registered_type.data::<ReflectComponent>().is_some() {
                response.push(registered_type.type_info().type_path().to_owned());
            }
        }
    }

    // Sort both for cleanliness and to reduce the risk that clients start
    // accidentally depending on the order.
    response.sort();

    serde_json::to_value(response).map_err(BrpError::internal)
}

/// Handles a `bevy/list_resources` request coming from a client.
pub fn process_remote_list_resources_request(
    In(_params): In<Option<Value>>,
    world: &World,
) -> BrpResult {
    let mut response = BrpListResourcesResponse::default();

    let app_type_registry = world.resource::<AppTypeRegistry>();
    let type_registry = app_type_registry.read();

    for registered_type in type_registry.iter() {
        if registered_type.data::<ReflectResource>().is_some() {
            response.push(registered_type.type_info().type_path().to_owned());
        }
    }

    response.sort();

    serde_json::to_value(response).map_err(BrpError::internal)
}

/// Handles a `bevy/list+watch` request coming from a client.
pub fn process_remote_list_watching_request(
    In(params): In<Option<Value>>,
    world: &World,
    mut removal_cursors: Local<HashMap<ComponentId, EventCursor<RemovedComponentEntity>>>,
) -> BrpResult<Option<Value>> {
    let BrpListParams { entity } = parse_some(params)?;
    let entity_ref = get_entity(world, entity)?;
    let mut response = BrpListWatchingResponse::default();

    for component_id in entity_ref.archetype().components() {
        let ticks = entity_ref
            .get_change_ticks_by_id(component_id)
            .ok_or(BrpError::internal("Failed to get ticks"))?;

        if ticks.is_added(world.last_change_tick(), world.read_change_tick()) {
            let Some(component_info) = world.components().get_info(component_id) else {
                continue;
            };
            response.added.push(component_info.name().to_owned());
        }
    }

    for (component_id, events) in world.removed_components().iter() {
        let cursor = removal_cursors
            .entry(*component_id)
            .or_insert_with(|| events.get_cursor());
        for event in cursor.read(events) {
            if Entity::from(event.clone()) == entity {
                let Some(component_info) = world.components().get_info(*component_id) else {
                    continue;
                };
                response.removed.push(component_info.name().to_owned());
            }
        }
    }

    if response.added.is_empty() && response.removed.is_empty() {
        Ok(None)
    } else {
        Ok(Some(
            serde_json::to_value(response).map_err(BrpError::internal)?,
        ))
    }
}

pub(crate) fn export_registry_types_typed(
    filter: BrpJsonSchemaQueryFilter,
    world: &World,
) -> Result<JsonSchemaBevyType, BrpError> {
    let metadata = world.resource::<crate::schemas::SchemaTypesMetadata>();
    let types = world.resource::<AppTypeRegistry>();
    let types = types.read();
    let mut schema = JsonSchemaBevyType {
        schema: Some(SchemaMarker.into()),
        description: Some(
            "This schema represents the types registered in the Bevy application.".into(),
        ),
        ..Default::default()
    };
    let definitions: Vec<TypeId> = types
        .iter()
        .filter_map(|type_reg| {
            if filter.should_skip_for_crate(type_reg) {
                return None;
            }
            let registered_types = metadata.get_registered_reflect_types(type_reg);

            if filter
                .type_limit
                .should_skip_type(registered_types.as_slice())
            {
                return None;
            }

            let type_id = type_reg.type_id();
            let mut dep_ids = types.get_type_dependencies(type_id);
            dep_ids.insert(type_id);
            Some(dep_ids)
        })
        .flatten()
        .collect();
    schema.definitions = definitions
        .into_iter()
        .flat_map(|id| {
            let result = types.build_schema_for_type_id(id, metadata);
            let Some((Some(schema_id), schema)) = result else {
                return None;
            };
            Some((schema_id, Box::new(schema)))
        })
        .collect();
    if filter.meta_schemas_export {
        schema.one_of = schema
            .definitions
            .iter()
            .flat_map(|(id, schema)| {
                if !schema.reflect_type_data.contains(&"Serialize".into())
                    || !schema.reflect_type_data.contains(&"Deserialize".into())
                {
                    return None;
                }
                let schema = JsonSchemaBevyType {
                    properties: [(
                        schema.type_path.clone(),
                        JsonSchemaBevyType {
                            ref_type: Some(TypeReferencePath::definition((*id).clone())),
                            description: schema.description.clone(),
                            ..Default::default()
                        }
                        .into(),
                    )]
                    .into(),
                    schema_type: Some(crate::schemas::json_schema::SchemaType::Object.into()),
                    additional_properties: Some(JsonSchemaVariant::BoolValue(false)),
                    description: schema.description.clone(),
                    reflect_type_data: schema.reflect_type_data.clone(),
                    ..Default::default()
                };
                Some(schema.into())
            })
            .collect();
    }
    Ok(schema)
}

/// Handles a `bevy/registry/schema` request (list all registry types in form of schema) coming from a client.
pub fn export_registry_types(In(params): In<Option<Value>>, world: &World) -> BrpResult {
    let filter: BrpJsonSchemaQueryFilter = match params {
        None => Default::default(),
        Some(params) => parse(params)?,
    };
    let result = export_registry_types_typed(filter, world)?;
    serde_json::to_value(result).map_err(BrpError::internal)
}

/// Immutably retrieves an entity from the [`World`], returning an error if the
/// entity isn't present.
fn get_entity(world: &World, entity: Entity) -> Result<EntityRef<'_>, BrpError> {
    world
        .get_entity(entity)
        .map_err(|_| BrpError::entity_not_found(entity))
}

/// Mutably retrieves an entity from the [`World`], returning an error if the
/// entity isn't present.
fn get_entity_mut(world: &mut World, entity: Entity) -> Result<EntityWorldMut<'_>, BrpError> {
    world
        .get_entity_mut(entity)
        .map_err(|_| BrpError::entity_not_found(entity))
}

/// Given components full path, returns a tuple that contains
/// - A list of corresponding [`TypeId`] and [`ComponentId`] for registered components.
/// - A list of unregistered component paths.
///
/// Note that the supplied path names must be *full* path names: e.g.
/// `bevy_transform::components::transform::Transform` instead of `Transform`.
fn get_component_ids(
    type_registry: &TypeRegistry,
    world: &World,
    component_paths: Vec<String>,
    strict: bool,
) -> AnyhowResult<(Vec<(TypeId, ComponentId)>, Vec<String>)> {
    let mut component_ids = vec![];
    let mut unregistered_components = vec![];

    for component_path in component_paths {
        let maybe_component_tuple = get_component_type_registration(type_registry, &component_path)
            .ok()
            .and_then(|type_registration| {
                let type_id = type_registration.type_id();
                world
                    .components()
                    .get_id(type_id)
                    .map(|component_id| (type_id, component_id))
            });
        if let Some((type_id, component_id)) = maybe_component_tuple {
            component_ids.push((type_id, component_id));
        } else if strict {
            return Err(anyhow!(
                "Component `{}` isn't registered or used in the world",
                component_path
            ));
        } else {
            unregistered_components.push(component_path);
        }
    }

    Ok((component_ids, unregistered_components))
}

/// Given an entity (`entity_ref`) and a list of reflected component information
/// (`paths_and_reflect_components`), return a map which associates each component to
/// its serialized value from the entity.
///
/// This is intended to be used on an entity which has already been filtered; components
/// where the value is not present on an entity are simply skipped.
fn build_components_map<'a>(
    entity_ref: FilteredEntityRef,
    paths_and_reflect_components: impl Iterator<Item = (&'a str, &'a ReflectComponent)>,
    type_registry: &TypeRegistry,
) -> AnyhowResult<HashMap<String, Value>> {
    let mut serialized_components_map = <HashMap<_, _>>::default();

    for (type_path, reflect_component) in paths_and_reflect_components {
        let Some(reflected) = reflect_component.reflect(entity_ref.clone()) else {
            continue;
        };

        let reflect_serializer =
            ReflectSerializer::new(reflected.as_partial_reflect(), type_registry);
        let Value::Object(serialized_object) = serde_json::to_value(&reflect_serializer)? else {
            return Err(anyhow!("Component `{}` could not be serialized", type_path));
        };

        serialized_components_map.extend(serialized_object.into_iter());
    }

    Ok(serialized_components_map)
}

/// Given an entity (`entity_ref`),
/// a list of reflected component information (`paths_and_reflect_components`)
/// and a list of unregistered components,
/// return a map which associates each component to a boolean value indicating
/// whether or not that component is present on the entity.
/// Unregistered components are considered absent from the entity.
fn build_has_map<'a>(
    entity_ref: FilteredEntityRef,
    paths_and_reflect_components: impl Iterator<Item = (&'a str, &'a ReflectComponent)>,
    unregistered_components: &[String],
) -> HashMap<String, Value> {
    let mut has_map = <HashMap<_, _>>::default();

    for (type_path, reflect_component) in paths_and_reflect_components {
        let has = reflect_component.contains(entity_ref.clone());
        has_map.insert(type_path.to_owned(), Value::Bool(has));
    }
    unregistered_components.iter().for_each(|component| {
        has_map.insert(component.to_owned(), Value::Bool(false));
    });

    has_map
}

/// Given a component ID, return the associated [type path] and `ReflectComponent` if possible.
///
/// The `ReflectComponent` part is the meat of this; the type path is only used for error messages.
///
/// [type path]: bevy_reflect::TypePath::type_path
fn reflect_component_from_id(
    component_type_id: TypeId,
    type_registry: &TypeRegistry,
) -> AnyhowResult<(&str, &ReflectComponent)> {
    let Some(type_registration) = type_registry.get(component_type_id) else {
        return Err(anyhow!(
            "Component `{:?}` isn't registered",
            component_type_id
        ));
    };

    let type_path = type_registration.type_info().type_path();

    let Some(reflect_component) = type_registration.data::<ReflectComponent>() else {
        return Err(anyhow!("Component `{}` isn't reflectable", type_path));
    };

    Ok((type_path, reflect_component))
}

/// Given a collection of component paths and their associated serialized values (`components`),
/// return the associated collection of deserialized reflected values.
fn deserialize_components(
    type_registry: &TypeRegistry,
    components: HashMap<String, BrpValue>,
) -> AnyhowResult<Vec<Box<dyn PartialReflect>>> {
    let mut reflect_components = vec![];

    for (component_path, component) in components {
        let Some(component_type) = type_registry.get_with_type_path(&component_path) else {
            return Err(anyhow!("Unknown component type: `{}`", component_path));
        };
        let reflected: Box<dyn PartialReflect> =
            TypedReflectDeserializer::new(component_type, type_registry)
                .deserialize(&component.0)
                .map_err(|err| anyhow!("{component_path} is invalid: {err}"))?;
        reflect_components.push(reflected);
    }

    Ok(reflect_components)
}

/// Given a resource path and an associated serialized value (`value`), return the
/// deserialized value.
fn deserialize_resource(
    type_registry: &TypeRegistry,
    resource_path: &str,
    value: Value,
) -> AnyhowResult<Box<dyn PartialReflect>> {
    let Some(resource_type) = type_registry.get_with_type_path(resource_path) else {
        return Err(anyhow!("Unknown resource type: `{}`", resource_path));
    };
    let reflected: Box<dyn PartialReflect> =
        TypedReflectDeserializer::new(resource_type, type_registry)
            .deserialize(&value)
            .map_err(|err| anyhow!("{resource_path} is invalid: {err}"))?;
    Ok(reflected)
}

/// Given a collection `reflect_components` of reflected component values, insert them into
/// the given entity (`entity_world_mut`).
fn insert_reflected_components(
    type_registry: &TypeRegistry,
    mut entity_world_mut: EntityWorldMut,
    reflect_components: Vec<Box<dyn PartialReflect>>,
) -> AnyhowResult<()> {
    for reflected in reflect_components {
        let reflect_component =
            get_reflect_component(type_registry, reflected.reflect_type_path())?;
        reflect_component.insert(&mut entity_world_mut, &*reflected, type_registry);
    }

    Ok(())
}

/// Given a component's type path, return the associated [`ReflectComponent`] from the given
/// `type_registry` if possible.
fn get_reflect_component<'r>(
    type_registry: &'r TypeRegistry,
    component_path: &str,
) -> AnyhowResult<&'r ReflectComponent> {
    let component_registration = get_component_type_registration(type_registry, component_path)?;

    component_registration
        .data::<ReflectComponent>()
        .ok_or_else(|| anyhow!("Component `{}` isn't reflectable", component_path))
}

/// Given a component's type path, return the associated [`TypeRegistration`] from the given
/// `type_registry` if possible.
fn get_component_type_registration<'r>(
    type_registry: &'r TypeRegistry,
    component_path: &str,
) -> AnyhowResult<&'r TypeRegistration> {
    type_registry
        .get_with_type_path(component_path)
        .ok_or_else(|| anyhow!("Unknown component type: `{}`", component_path))
}

/// Given a resource's type path, return the associated [`ReflectResource`] from the given
/// `type_registry` if possible.
fn get_reflect_resource<'r>(
    type_registry: &'r TypeRegistry,
    resource_path: &str,
) -> AnyhowResult<&'r ReflectResource> {
    let resource_registration = get_resource_type_registration(type_registry, resource_path)?;

    resource_registration
        .data::<ReflectResource>()
        .ok_or_else(|| anyhow!("Resource `{}` isn't reflectable", resource_path))
}

/// Given a resource's type path, return the associated [`TypeRegistration`] from the given
/// `type_registry` if possible.
fn get_resource_type_registration<'r>(
    type_registry: &'r TypeRegistry,
    resource_path: &str,
) -> AnyhowResult<&'r TypeRegistration> {
    type_registry
        .get_with_type_path(resource_path)
        .ok_or_else(|| anyhow!("Unknown resource type: `{}`", resource_path))
}

#[cfg(test)]
mod tests {
    /// A generic function that tests serialization and deserialization of any type
    /// implementing Serialize and Deserialize traits.
    fn test_serialize_deserialize<T>(value: T)
    where
        T: Serialize + for<'a> Deserialize<'a> + PartialEq + core::fmt::Debug,
    {
        // Serialize the value to JSON string
        let serialized = serde_json::to_string(&value).expect("Failed to serialize");

        // Deserialize the JSON string back into the original type
        let deserialized: T = serde_json::from_str(&serialized).expect("Failed to deserialize");

        // Assert that the deserialized value is the same as the original
        assert_eq!(
            &value, &deserialized,
            "Deserialized value does not match original"
        );
    }

    use bevy_ecs::component::Component;
    use bevy_reflect::Reflect;

    use crate::schemas::{RegisterReflectJsonSchemas, SchemaTypesMetadata};

    use super::*;

    #[test]
    fn serialization_tests() {
        test_serialize_deserialize(BrpQueryRow {
            components: Default::default(),
            entity: Entity::from_raw(0),
            has: Default::default(),
        });
        test_serialize_deserialize(BrpListWatchingResponse::default());
        test_serialize_deserialize(BrpQuery::default());
        test_serialize_deserialize(BrpJsonSchemaQueryFilter::default());
        test_serialize_deserialize(BrpJsonSchemaQueryFilter {
            type_limit: JsonSchemaTypeLimit {
                with: vec!["Resource".to_owned()],
                ..Default::default()
            },
            ..Default::default()
        });
        test_serialize_deserialize(BrpListParams {
            entity: Entity::from_raw(0),
        });
    }

    #[test]
    fn reflection_serialization_tests() {
        use bevy_ecs::resource::Resource;

        #[derive(Reflect, Default, Deserialize, Serialize, Resource)]
        #[reflect(Resource)]
        pub struct ResourceStruct {
            /// FIELD DOC
            pub field: String,
            pub second_field: Option<(u8, Option<u16>)>,
        }
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct OtherStruct {
            /// FIELD DOC
            pub field: String,
            pub second_field: Option<(u8, Option<u16>)>,
        }
        /// STRUCT DOC
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct SecondStruct;
        /// STRUCT DOC
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct ThirdStruct {
            pub array_strings: Vec<String>,
            pub array_structs: [OtherStruct; 5],
            // pub map_strings: HashMap<String, i32>,
        }
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct NestedStruct {
            pub other: OtherStruct,
            pub second: SecondStruct,
            /// DOC FOR FIELD
            pub third: ThirdStruct,
        }
        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<NestedStruct>();
            register.register::<ResourceStruct>();
            register.register::<bevy_math::Vec3>();
            register.registry_force_schema_to_be_array::<bevy_math::Vec3>();
        }
        let value = NestedStruct {
            other: OtherStruct {
                field: "S".into(),
                second_field: Some((0, None)),
            },
            second: SecondStruct,
            third: ThirdStruct {
                array_strings: ["s".into(), "SS".into()].into(),
                array_structs: [
                    OtherStruct::default(),
                    OtherStruct::default(),
                    OtherStruct::default(),
                    OtherStruct::default(),
                    OtherStruct::default(),
                ],
            },
        };
        let mut world = World::new();
        world.insert_resource(atr.clone());
        world.insert_resource(SchemaTypesMetadata::default());
        let response = export_registry_types_ext(BrpJsonSchemaQueryFilter::default(), &world);
        let schema_value = serde_json::to_value(response).expect("Failed to serialize schema");
        let type_registry = atr.read();
        let serializer = ReflectSerializer::new(&value, &type_registry);
        let res = ResourceStruct {
            field: "SET".into(),
            second_field: Some((45, None)),
        };
        let serializer_2 = ReflectSerializer::new(&res, &type_registry);
        let json = serde_json::to_value(&serializer).expect("msg");
        let validator = jsonschema::options()
            .with_draft(jsonschema::Draft::Draft202012)
            .build(&schema_value)
            .expect("Failed to validate json schema");
        assert!(validator.validate(&json).is_ok());
        let json = serde_json::to_value(&serializer_2).expect("msg");
        eprintln!(
            "{}",
            serde_json::to_string_pretty(&json).unwrap_or_default()
        );
        assert!(validator.validate(&json).is_ok());
    }

    #[test]
    #[cfg(feature = "bevy_math")]
    fn export_schema_test() {
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct OtherStruct {
            /// FIELD DOC
            pub field: String,
            pub second_field: Option<(u8, Option<u16>)>,
        }
        /// STRUCT DOC
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct SecondStruct;
        /// STRUCT DOC
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct ThirdStruct {
            pub array_strings: Vec<String>,
            pub array_structs: [OtherStruct; 5],
            // pub map_strings: HashMap<String, i32>,
        }
        #[derive(Reflect, Default, Deserialize, Serialize, Component)]
        #[reflect(Component)]
        pub struct NestedStruct {
            pub other: OtherStruct,
            pub second: SecondStruct,
            /// DOC FOR FIELD
            pub third: ThirdStruct,
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<NestedStruct>();
            register.register::<bevy_math::Vec3>();
            register.registry_force_schema_to_be_array::<bevy_math::Vec3>();
        }
        let mut world = World::new();
        world.insert_resource(atr);
        world.insert_resource(SchemaTypesMetadata::default());
        let response = export_registry_types_ext(BrpJsonSchemaQueryFilter::default(), &world);
        let schema_value = serde_json::to_value(&response).unwrap();
        _ = jsonschema::options()
            .with_draft(jsonschema::Draft::Draft202012)
            .build(&schema_value)
            .expect("Failed to validate json schema");
        assert_eq!(
            response.definitions.len(),
            6,
            "Expected 6 definitions, got: {}: {}",
            response.definitions.keys().len(),
            serde_json::to_string_pretty(&response).unwrap_or_default()
        );
        let response = export_registry_types_ext(
            BrpJsonSchemaQueryFilter {
                with_crates: vec!["glam".to_string()],
                ..Default::default()
            },
            &world,
        );
        assert_eq!(
            response.definitions.len(),
            1,
            "Expected 1 definition, got: {}: {:?}",
            response.definitions.keys().len(),
            response.definitions.keys()
        );
        {
            use crate::schemas::reflect_info::TypeReferenceId;

            let first = response.definitions.iter().next().expect("Should have one");
            assert_eq!(first.0, &TypeReferenceId::from("glam::Vec3"));
        }
        let response = export_registry_types_ext(
            BrpJsonSchemaQueryFilter {
                with_crates: vec!["bevy_remote".to_string()],
                ..Default::default()
            },
            &world,
        );
        assert_eq!(
            response.definitions.len(),
            5,
            "Expected 5 definitions, got: {}: {:?}",
            response.definitions.len(),
            response.definitions.keys()
        );
        let response = export_registry_types_ext(
            BrpJsonSchemaQueryFilter {
                type_limit: JsonSchemaTypeLimit {
                    with: vec!["Component".to_string()],
                    ..Default::default()
                },
                ..Default::default()
            },
            &world,
        );
        assert_eq!(
            response.definitions.len(),
            5,
            "Expected 5 definitions, got: {}: {:?}",
            response.definitions.len(),
            response.definitions.keys()
        );
    }

    fn export_registry_types_ext(
        input: BrpJsonSchemaQueryFilter,
        world: &World,
    ) -> JsonSchemaBevyType {
        export_registry_types_typed(input, world).expect("Failed to export registry types")
    }
}

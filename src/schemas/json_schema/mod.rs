//! Module with JSON Schema type for Bevy Registry Types.
//!  It tries to follow this standard: <https://json-schema.org/specification>

use crate::DataTypes;
use bevy_platform::collections::HashMap;
use bevy_reflect::{Reflect, TypeInfo, TypeRegistry};
use draft_7::{
    BasicTypeInfoBuilder, JsonSchemaBasic, SchemaMarker, SchemaType, serialize_schema_url,
};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::any::TypeId;

pub mod draft_7;
pub mod reflect_helper;

pub fn export_type_json_schema(
    type_registry: &TypeRegistry,
    type_id: TypeId,
    data_types: &DataTypes,
) -> Option<JsonSchemaBevyType> {
    let base_type = type_registry.build_json_schema(type_id)?;

    Some(JsonSchemaBevyType::build_from(
        type_id,
        &base_type,
        type_registry,
        data_types,
    ))
}

/// Export type JSON Schema with definitions.
/// It can be useful for generating schemas for assets validation.
pub fn export_type_json_schema_with_definitions(
    type_registry: &TypeRegistry,
    type_id: TypeId,
    data_types: &DataTypes,
) -> Option<JsonSchemaBevyType> {
    let base_schema = type_registry.build_json_schema(type_id)?;
    let base_schema = type_registry.add_definitions(&base_schema);

    Some(JsonSchemaBevyType::build_from(
        type_id,
        &base_schema,
        type_registry,
        data_types,
    ))
}

/// JSON Schema type for Bevy Registry Types
/// It tries to follow this standard: <https://json-schema.org/specification>
///
/// To take the full advantage from info provided by Bevy registry it provides extra fields
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default, Reflect)]
#[serde(rename_all = "camelCase")]
pub struct JsonSchemaBevyType {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[serde(rename = "$schema", serialize_with = "serialize_schema_url")]
    pub schema: Option<SchemaMarker>,
    /// Bevy specific field, short path of the type.
    pub short_path: String,
    /// Bevy specific field, full path of the type.
    pub type_path: String,
    /// Bevy specific field, path of the module that type is part of.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub module_path: Option<String>,
    /// Bevy specific field, name of the crate that type is part of.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub crate_name: Option<String>,
    /// Bevy specific field, names of the types that type reflects.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub reflect_types: Vec<String>,
    /// Bevy specific field, [`TypeInfo`] type mapping.
    pub kind: SchemaKind,
    /// Bevy specific field, provided when [`SchemaKind`] `kind` field is equal to [`SchemaKind::Map`].
    ///
    /// It contains type info of key of the Map.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[reflect(ignore)]
    pub key_type: Option<Value>,
    /// Bevy specific field, provided when [`SchemaKind`] `kind` field is equal to [`SchemaKind::Map`].
    ///
    /// It contains type info of value of the Map.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[reflect(ignore)]
    pub value_type: Option<Value>,
    /// The type keyword is fundamental to JSON Schema. It specifies the data type for a schema.
    #[serde(rename = "type")]
    pub schema_type: SchemaType,
    /// The behavior of this keyword depends on the presence and annotation results of "properties"
    /// and "patternProperties" within the same schema object.
    /// Validation with "additionalProperties" applies only to the child
    /// values of instance names that do not appear in the annotation results of either "properties" or "patternProperties".
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub additional_properties: Option<bool>,
    /// Validation succeeds if, for each name that appears in both the instance and as a name
    /// within this keyword's value, the child instance for that name successfully validates
    /// against the corresponding schema.
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    #[reflect(ignore)]
    pub properties: HashMap<String, Value>,
    /// An object instance is valid against this keyword if every item in the array is the name of a property in the instance.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub required: Vec<String>,
    /// An instance validates successfully against this keyword if it validates successfully against exactly one schema defined by this keyword's value.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    #[reflect(ignore)]
    pub one_of: Vec<Value>,
    /// Validation succeeds if each element of the instance validates against the schema at the same position, if any. This keyword does not constrain the length of the array. If the array is longer than this keyword's value, this keyword validates only the prefix of matching length.
    ///
    /// This keyword produces an annotation value which is the largest index to which this keyword
    /// applied a subschema. The value MAY be a boolean true if a subschema was applied to every
    /// index of the instance, such as is produced by the "items" keyword.
    /// This annotation affects the behavior of "items" and "unevaluatedItems".
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    #[reflect(ignore)]
    pub prefix_items: Vec<Value>,
    /// This keyword applies its subschema to all instance elements at indexes greater
    /// than the length of the "prefixItems" array in the same schema object,
    /// as reported by the annotation result of that "prefixItems" keyword.
    /// If no such annotation result exists, "items" applies its subschema to all
    /// instance array elements.
    ///
    /// If the "items" subschema is applied to any positions within the instance array,
    /// it produces an annotation result of boolean true, indicating that all remaining
    /// array elements have been evaluated against this keyword's subschema.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[reflect(ignore)]
    pub items: Option<Value>,
    // The value of this keyword MUST be a non-negative integer.
    // An array instance is valid against "minItems" if its size is greater than,
    // or equal to, the value of this keyword.
    // Omitting this keyword has the same behavior as a value of 0.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub min_items: Option<usize>,
    // The value of this keyword MUST be a non-negative integer.
    // An array instance is valid against "maxItems" if its size is less than,
    // or equal to, the value of this keyword.
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub max_items: Option<usize>,
    /// Type description
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,
    /// The value of "maximum" MUST be a number,
    /// representing an inclusive upper limit for a numeric instance.
    /// If the instance is a number, then this keyword validates only
    /// if the instance is less than or exactly equal to "maximum".
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub maximum: Option<i64>,
    /// The value of "minimum" MUST be a number,
    /// representing an inclusive lower limit for a numeric instance.
    /// If the instance is a number, then this keyword validates only
    /// if the instance is greater than or exactly equal to "minimum".
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub minimum: Option<i64>,
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    #[reflect(ignore)]
    pub definitions: HashMap<String, Box<JsonSchemaBevyType>>,
}
impl JsonSchemaBevyType {
    pub fn build_from(
        type_id: TypeId,
        base_schema: &JsonSchemaBasic,
        type_registry: &TypeRegistry,
        data_types: &DataTypes,
    ) -> Self {
        let type_reg = type_registry.get(type_id).expect("msg");

        let t = type_reg.type_info();
        let binding = t.type_path_table();

        let short_path = binding.short_path();
        let type_path = binding.path();
        let mut typed_schema = JsonSchemaBevyType {
            reflect_types: data_types.get_registered_reflect_types(type_reg),
            short_path: short_path.to_owned(),
            type_path: type_path.to_owned(),
            crate_name: binding.crate_name().map(str::to_owned),
            module_path: binding.module_path().map(str::to_owned),
            ..Self::from(base_schema)
        };
        match t {
            TypeInfo::Struct(_) => {
                typed_schema.kind = SchemaKind::Struct;
            }
            TypeInfo::Enum(_info) => {
                typed_schema.kind = SchemaKind::Enum;
            }
            TypeInfo::TupleStruct(_) => {
                typed_schema.kind = SchemaKind::TupleStruct;
            }
            TypeInfo::List(_) => {
                typed_schema.kind = SchemaKind::List;
            }
            TypeInfo::Array(_) => {
                typed_schema.kind = SchemaKind::Array;
            }
            TypeInfo::Map(info) => {
                typed_schema.kind = SchemaKind::Map;
                typed_schema.key_type = JsonSchemaBasic::build_json(&info.key_ty()).into();
                typed_schema.value_type = JsonSchemaBasic::build_json(&info.value_ty()).into();
            }
            TypeInfo::Tuple(_) => {
                typed_schema.kind = SchemaKind::Tuple;
            }
            TypeInfo::Set(_) => {
                typed_schema.kind = SchemaKind::Set;
            }
            TypeInfo::Opaque(_) => {
                typed_schema.kind = SchemaKind::Value;
            }
        };
        typed_schema
    }
}

impl From<&JsonSchemaBasic> for JsonSchemaBevyType {
    fn from(value: &JsonSchemaBasic) -> Self {
        let items: Option<Value> = if value.items.is_empty() {
            None
        } else {
            Some(value.items.iter().map(|f| f.to_value()).collect())
        };
        JsonSchemaBevyType {
            schema: value.schema.clone(),
            schema_type: value.r#type.clone().unwrap_or_default(),
            additional_properties: value.additional_properties.clone(),
            properties: value
                .properties
                .iter()
                .map(|(k, v)| (k.clone(), v.to_value()))
                .collect(),
            definitions: value
                .definitions
                .iter()
                .map(|(k, v)| (k.clone(), Box::new((&**v).into())))
                .collect(),
            required: value.required.clone(),
            one_of: value
                .one_of
                .iter()
                .map(|s| serde_json::to_value(s))
                .flatten()
                .collect(),
            prefix_items: value.prefix_items.iter().map(|f| f.to_value()).collect(),
            items,
            min_items: value.min_items,
            max_items: value.max_items,
            description: value.description.clone(),
            maximum: value.maximum,
            minimum: value.minimum,
            ..Default::default()
        }
    }
}

/// Kind of json schema, maps [`TypeInfo`] type
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default, Reflect)]
pub enum SchemaKind {
    /// Struct
    #[default]
    Struct,
    /// Enum type
    Enum,
    /// A key-value map
    Map,
    /// Array
    Array,
    /// List
    List,
    /// Fixed size collection of items
    Tuple,
    /// Fixed size collection of items with named fields
    TupleStruct,
    /// Set of unique values
    Set,
    /// Single value, eg. primitive types
    Value,
}

#[cfg(test)]
mod tests {
    use std::any::TypeId;

    use super::*;
    use bevy_ecs::reflect::{ReflectComponent, ReflectResource};
    use bevy_ecs::{component::Component, reflect::AppTypeRegistry, resource::Resource};
    use bevy_reflect::Reflect;
    use bevy_reflect::{ReflectDeserialize, ReflectSerialize, prelude::ReflectDefault};
    use serde_json::json;

    #[test]
    fn reflect_export_struct() {
        #[derive(Reflect, Resource, Default, Deserialize, Serialize)]
        #[reflect(Resource, Default, Serialize, Deserialize)]
        struct Foo {
            a: f32,
            b: Option<f32>,
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<Foo>();
        }
        let type_registry = atr.read();
        let Some(schema) =
            export_type_json_schema(&type_registry, TypeId::of::<Foo>(), &DataTypes::default())
        else {
            panic!("Failed to export type");
        };

        assert!(
            !schema.reflect_types.contains(&"Component".to_owned()),
            "Should not be a component"
        );
        assert!(
            schema.reflect_types.contains(&"Resource".to_owned()),
            "Should be a resource"
        );
        let _ = schema.properties.get("a").expect("Missing `a` field");
        let _ = schema.properties.get("b").expect("Missing `b` field");
        assert!(
            schema.required.contains(&"a".to_owned()),
            "Field a should be required"
        );
        assert!(
            !schema.required.contains(&"b".to_owned()),
            "Field b should not be required"
        );
    }

    #[test]
    fn reflect_export_enum() {
        #[derive(Reflect, Component, Default, Deserialize, Serialize)]
        #[reflect(Component, Default, Serialize, Deserialize)]
        enum EnumComponent {
            ValueOne(i32),
            ValueTwo {
                test: i32,
            },
            #[default]
            NoValue,
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<EnumComponent>();
        }
        let type_registry = atr.read();
        let Some(schema) = export_type_json_schema(
            &type_registry,
            TypeId::of::<EnumComponent>(),
            &DataTypes::default(),
        ) else {
            panic!("Failed to export type");
        };
        eprintln!("Schema: {:#?}", schema);
        assert!(
            schema.reflect_types.contains(&"Component".to_owned()),
            "Should be a component"
        );
        assert!(
            !schema.reflect_types.contains(&"Resource".to_owned()),
            "Should not be a resource"
        );
        assert!(schema.properties.is_empty(), "Should not have any field");
        assert!(schema.one_of.len() == 3, "Should have 3 possible schemas");
    }

    #[test]
    fn reflect_export_struct_without_reflect_types() {
        #[derive(Reflect, Component, Default, Deserialize, Serialize)]
        enum EnumComponent {
            ValueOne(i32),
            ValueTwo {
                test: i32,
            },
            #[default]
            NoValue,
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<EnumComponent>();
        }
        let type_registry = atr.read();
        let Some(schema) = export_type_json_schema(
            &type_registry,
            TypeId::of::<EnumComponent>(),
            &DataTypes::default(),
        ) else {
            panic!("Failed to export EnumComponent");
        };
        assert!(
            !schema.reflect_types.contains(&"Component".to_owned()),
            "Should not be a component"
        );
        assert!(
            !schema.reflect_types.contains(&"Resource".to_owned()),
            "Should not be a resource"
        );
        assert!(schema.properties.is_empty(), "Should not have any field");
        assert!(schema.one_of.len() == 3, "Should have 3 possible schemas");
    }

    #[test]
    fn reflect_export_tuple_struct() {
        #[derive(Reflect, Component, Default, Deserialize, Serialize)]
        #[reflect(Component, Default, Serialize, Deserialize)]
        struct TupleStructType(usize, i32);

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<TupleStructType>();
        }
        let type_registry = atr.read();
        let Some(schema) = export_type_json_schema(
            &type_registry,
            TypeId::of::<TupleStructType>(),
            &DataTypes::default(),
        ) else {
            panic!("Failed to export type");
        };
        assert!(
            schema.reflect_types.contains(&"Component".to_owned()),
            "Should be a component"
        );
        assert!(
            !schema.reflect_types.contains(&"Resource".to_owned()),
            "Should not be a resource"
        );
        assert!(schema.properties.is_empty(), "Should not have any field");
        assert!(schema.prefix_items.len() == 2, "Should have 2 prefix items");
    }

    #[test]
    fn reflect_export_serialization_check() {
        /// TEST DOCS
        #[derive(Reflect, Resource, Default, Deserialize, Serialize)]
        #[reflect(Resource, Default)]
        struct Foo {
            /// THIS IS A FIELD
            a: i32,
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<Foo>();
        }
        let type_registry = atr.read();
        let Some(schema) =
            export_type_json_schema(&type_registry, TypeId::of::<Foo>(), &DataTypes::default())
        else {
            panic!("Failed to export type");
        };
        let schema_as_value = serde_json::to_value(&schema).expect("Should serialize");
        eprintln!("{:?}", &schema_as_value);
        let value = json!({
          "$schema": "https://json-schema.org/draft-07/schema",
          "description": "TEST DOCS",
          "shortPath": "Foo",
          "typePath": "bevy_remote::schemas::json_schema::tests::Foo",
          "modulePath": "bevy_remote::schemas::json_schema::tests",
          "crateName": "bevy_remote",
          "reflectTypes": [
            "Resource",
            "Default",
          ],
          "kind": "Struct",
          "type": "object",
          "additionalProperties": false,
          "properties": {
            "a": {
              "description": "THIS IS A FIELD",
              "type": "integer",
              "maximum": i32::MAX,
              "minimum": i32::MIN,
            },
          },
          "required": [
            "a"
          ]
        });
        assert_eq!(schema_as_value, value);
    }

    #[test]
    fn reflect_export_enum_serialization_check() {
        /// TEST DOCS
        #[derive(Reflect, Default, Deserialize, Serialize, Hash)]
        #[reflect(Hash, Default)]
        pub enum ParticleTextType {
            #[default]
            NoInfo,
            LevelUp,
            Damage(usize),
            Gold(usize),
            GoldWithExtra(usize, usize),
        }

        let atr = AppTypeRegistry::default();
        {
            let mut register = atr.write();
            register.register::<ParticleTextType>();
        }
        let type_registry = atr.read();
        let Some(schema) = export_type_json_schema(
            &type_registry,
            TypeId::of::<ParticleTextType>(),
            &DataTypes::default(),
        ) else {
            panic!("Failed to export type");
        };
        let schema_as_value = serde_json::to_value(&schema).expect("Should serialize");
        eprintln!("{:#?}", &schema_as_value);
    }
}

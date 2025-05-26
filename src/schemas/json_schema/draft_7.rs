//! JSON Schema Draft 7 types
//! [JSON Schema Draft 7](https://json-schema.org/draft-07/draft-handrews-json-schema-01)

use bevy_platform::collections::{HashMap, HashSet};
use bevy_reflect::{
    NamedField, Reflect, Type, TypeInfo, TypeRegistration, TypeRegistry, UnnamedField, VariantInfo,
};
use core::fmt;
use serde::{
    Deserialize, Deserializer, Serialize, Serializer,
    de::{self, Visitor},
};
use serde_json::Value;
use std::{
    any::TypeId,
    fmt::{Display, Formatter},
};

use crate::ReflectSerializeAsArray;

use super::reflect_helper::{MinMaxTypeReflectHelper, ReflectDocReader};

/// Type of json schema
/// More [here](https://json-schema.org/draft-07/draft-handrews-json-schema-01#rfc.section.4.2.1)
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default, Reflect)]
#[serde(rename_all = "lowercase")]
pub enum SchemaType {
    /// A string of Unicode code points, from the JSON "string" production.
    String,

    /// An arbitrary-precision, base-10 decimal number value, from the JSON "number" production.
    Number,

    /// Represents both a signed and unsigned integer.
    Integer,

    /// An unordered set of properties mapping a string to an instance, from the JSON "object" production.
    Object,

    /// An ordered list of instances, from the JSON "array" production.
    Array,

    /// A "true" or "false" value, from the JSON "true" or "false" productions.
    Boolean,

    /// A JSON "null" production.
    #[default]
    Null,
}

impl SchemaType {
    /// Returns the primitive type corresponding to the given type ID, if it exists.
    pub fn try_get_primitive_type_from_type_id(type_id: TypeId) -> Option<Self> {
        if type_id.eq(&TypeId::of::<bool>()) {
            Some(Self::Boolean)
        } else if type_id.eq(&TypeId::of::<f32>()) || type_id.eq(&TypeId::of::<f64>()) {
            Some(Self::Number)
        } else if type_id.eq(&TypeId::of::<u8>())
            || type_id.eq(&TypeId::of::<u16>())
            || type_id.eq(&TypeId::of::<u32>())
            || type_id.eq(&TypeId::of::<u64>())
            || type_id.eq(&TypeId::of::<u128>())
            || type_id.eq(&TypeId::of::<usize>())
        {
            Some(Self::Integer)
        } else if type_id.eq(&TypeId::of::<i8>())
            || type_id.eq(&TypeId::of::<i16>())
            || type_id.eq(&TypeId::of::<i32>())
            || type_id.eq(&TypeId::of::<i64>())
            || type_id.eq(&TypeId::of::<i128>())
            || type_id.eq(&TypeId::of::<isize>())
        {
            Some(Self::Integer)
        } else if type_id.eq(&TypeId::of::<str>())
            || type_id.eq(&TypeId::of::<char>())
            || type_id.eq(&TypeId::of::<String>())
        {
            Some(Self::String)
        } else {
            None
        }
    }
}

#[derive(
    Debug, Deserialize, Clone, Copy, PartialEq, Default, Reflect, Hash, Eq, Ord, PartialOrd,
)]
#[serde(rename_all = "lowercase")]
pub enum ReferenceLocalization {
    #[default]
    /// used by json schema draft 7
    Definitions,
    /// used by OpenRPC
    Components,
}

impl Display for ReferenceLocalization {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReferenceLocalization::Definitions => write!(f, "#/definitions/"),
            ReferenceLocalization::Components => write!(f, "#/components/"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Default, Reflect, Hash, Eq, Ord, PartialOrd)]
pub struct TypeReferencePath(pub String, pub ReferenceLocalization);

impl TypeReferencePath {
    pub fn definition(t: &Type) -> Self {
        TypeReferencePath::new(t.path(), ReferenceLocalization::Definitions)
    }
    pub fn new(type_path: &str, l: ReferenceLocalization) -> Self {
        TypeReferencePath(type_path.replace("::", "-"), l)
    }
    pub fn type_path(&self) -> String {
        self.0.replace("-", "::")
    }

    pub fn change_localization(&mut self, new_localization: ReferenceLocalization) {
        self.1 = new_localization;
    }
}
impl Display for TypeReferencePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.1, self.0)
    }
}

impl Serialize for TypeReferencePath {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        serializer.serialize_str(&format!("{}", self))
    }
}

struct TypeReferencePathVisitor;

impl<'de> Visitor<'de> for TypeReferencePathVisitor {
    type Value = TypeReferencePath;

    fn expecting(&self, formatter: &mut fmt::Formatter) -> fmt::Result {
        formatter.write_str("an string with a '#' prefix")
    }

    fn visit_str<E>(self, value: &str) -> Result<Self::Value, E>
    where
        E: de::Error,
    {
        if let Some(definition) =
            value.strip_prefix(&ReferenceLocalization::Definitions.to_string())
        {
            Ok(TypeReferencePath(
                definition.to_string(),
                ReferenceLocalization::Definitions,
            ))
        } else if let Some(component) =
            value.strip_prefix(&ReferenceLocalization::Components.to_string())
        {
            Ok(TypeReferencePath(
                component.to_string(),
                ReferenceLocalization::Components,
            ))
        } else {
            Err(E::custom("Invalid reference path"))
        }
    }
}
impl<'de> Deserialize<'de> for TypeReferencePath {
    fn deserialize<D>(deserializer: D) -> Result<TypeReferencePath, D::Error>
    where
        D: Deserializer<'de>,
    {
        deserializer.deserialize_str(TypeReferencePathVisitor)
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
#[serde(untagged)]
pub enum JsonSchemaVariant {
    Const {
        #[reflect(ignore)]
        #[serde(rename = "const")]
        value: Value,
    },
    Schema(#[reflect(ignore)] Box<JsonSchemaBasic>),
}

impl JsonSchemaVariant {
    pub fn const_value(serializable: impl Serialize) -> Self {
        Self::Const {
            value: serde_json::to_value(serializable).unwrap_or_default(),
        }
    }
}

#[derive(Deserialize, Serialize, Debug, Default, Reflect, PartialEq, Clone, Copy)]
pub struct SchemaMarker;

pub(crate) fn serialize_schema_url<S>(
    _: &Option<SchemaMarker>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: Serializer,
{
    serializer.serialize_str("https://json-schema.org/draft-07/schema")
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default, Reflect)]
#[serde(rename_all = "lowercase")]
pub struct JsonSchemaBasic {
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[serde(rename = "$schema", serialize_with = "serialize_schema_url")]
    pub schema: Option<SchemaMarker>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub r#type: Option<SchemaType>,
    #[serde(rename = "$ref")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub ref_type: Option<TypeReferencePath>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub minimum: Option<i64>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub maximum: Option<i64>,
    /// Type description
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    #[reflect(ignore)]
    pub items: Vec<Box<JsonSchemaBasic>>,
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
    /// Validation succeeds if, for each name that appears in both the instance and as a name
    /// within this keyword's value, the child instance for that name successfully validates
    /// against the corresponding schema.
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    #[reflect(ignore)]
    pub properties: HashMap<String, Box<JsonSchemaBasic>>,
    /// An object instance is valid against this keyword if every item in the array is the name of a property in the instance.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub required: Vec<String>,
    /// The value of this keyword MUST be an array. This array SHOULD have at least one element. Elements in the array SHOULD be unique.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub r#enum: Vec<String>,
    /// An instance validates successfully against this keyword if it validates successfully against exactly one schema defined by this keyword's value.
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    #[reflect(ignore)]
    pub one_of: Vec<JsonSchemaVariant>,
    /// Validation succeeds if each element of the instance validates against the schema at the same position, if any. This keyword does not constrain the length of the array. If the array is longer than this keyword's value, this keyword validates only the prefix of matching length.
    ///
    /// This keyword produces an annotation value which is the largest index to which this keyword
    /// applied a subschema. The value MAY be a boolean true if a subschema was applied to every
    /// index of the instance, such as is produced by the "items" keyword.
    /// This annotation affects the behavior of "items" and "unevaluatedItems".
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    #[reflect(ignore)]
    pub prefix_items: Vec<Box<JsonSchemaBasic>>,
    /// The behavior of this keyword depends on the presence and annotation results of "properties"
    /// and "patternProperties" within the same schema object.
    /// Validation with "additionalProperties" applies only to the child
    /// values of instance names that do not appear in the annotation results of either "properties" or "patternProperties".
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub additional_properties: Option<bool>,
    /// Enforces a single allowed value for the instance.
    #[serde(rename = "const", skip_serializing_if = "Option::is_none", default)]
    pub const_value: Option<JsonSchemaVariant>,
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    #[reflect(ignore)]
    pub definitions: HashMap<String, Box<JsonSchemaBasic>>,
}

impl JsonSchemaBasic {
    pub fn get_referenced_types(&self) -> HashSet<TypeReferencePath> {
        let mut types = HashSet::new();
        if let Some(ref_type) = &self.ref_type {
            types.insert(ref_type.clone());
        }
        for prefix_item in &self.prefix_items {
            types.extend(prefix_item.get_referenced_types());
        }
        for item in &self.items {
            types.extend(item.get_referenced_types());
        }
        for (_, item) in &self.properties {
            types.extend(item.get_referenced_types());
        }
        for (_, item) in &self.definitions {
            types.extend(item.get_referenced_types());
        }
        for item in self.one_of.iter() {
            if let JsonSchemaVariant::Schema(schema) = item {
                types.extend(schema.get_referenced_types());
            }
        }
        if let Some(JsonSchemaVariant::Schema(schema)) = &self.const_value {
            types.extend(schema.get_referenced_types());
        }
        types
    }
    pub fn change_referenced_types_location(&mut self, location: ReferenceLocalization) {
        if let Some(ref_type) = &mut self.ref_type {
            ref_type.change_localization(location);
        }
        for prefix_item in &mut self.prefix_items {
            prefix_item.change_referenced_types_location(location);
        }
        for item in &mut self.items {
            item.change_referenced_types_location(location);
        }
        for (_, item) in &mut self.properties {
            item.change_referenced_types_location(location);
        }
        for (_, item) in &mut self.definitions {
            item.change_referenced_types_location(location);
        }
        for item in self.one_of.iter_mut() {
            if let JsonSchemaVariant::Schema(schema) = item {
                schema.change_referenced_types_location(location);
            }
        }
        if let Some(JsonSchemaVariant::Schema(schema)) = &mut self.const_value {
            schema.change_referenced_types_location(location);
        }
    }
    pub fn set_fixed_array(
        &mut self,
        fields: core::slice::Iter<'_, UnnamedField>,
        max_items: Option<usize>,
        min_items: Option<usize>,
    ) {
        self.properties.clear();
        self.required.clear();
        self.items.clear();
        self.prefix_items.clear();
        self.prefix_items = fields
            .map(|f| Box::new(JsonSchemaBasic::build(f)))
            .collect::<Vec<_>>();
        self.max_items = max_items;
        self.min_items = min_items;
    }
    pub fn set_properties(&mut self, fields: core::slice::Iter<'_, NamedField>) {
        self.properties.clear();
        self.required.clear();
        for field in fields {
            let data = if let Some(type_info) = field
                .type_info()
                .and_then(|f| JsonSchemaBasic::get_type_from_optional(f))
            {
                JsonSchemaBasic::build(&type_info)
            } else {
                self.required.push(field.name().to_owned());
                JsonSchemaBasic::build(field)
            };
            self.properties
                .insert(field.name().to_owned(), Box::new(data));
        }
    }
    pub fn set_type(&mut self, ty: SchemaType) {
        self.r#type = Some(ty);
        self.ref_type = None;
    }
    pub fn build(v: impl Into<JsonSchemaBasic>) -> Self {
        v.into()
    }

    pub fn build_json(v: impl Into<JsonSchemaBasic>) -> Value {
        v.into().to_value()
    }

    pub fn to_array_with_one_value(&self) -> Value {
        serde_json::to_value(vec![self]).unwrap_or_default()
    }

    pub fn to_value(&self) -> Value {
        serde_json::to_value(self).unwrap_or_default()
    }

    pub fn from_type_info(t: &TypeInfo, description: Option<String>) -> Self {
        if let Some(optional_type) = Self::get_type_from_optional(t) {
            return Self::build(&optional_type);
        }
        let mut info = Self::from_type(t.ty(), description);
        if let Some(array_el) = Self::try_get_array_info(t) {
            info.set_type(SchemaType::Array);
            info.items.push(Box::new(array_el));
        }
        info
    }

    pub fn from_type(t: &Type, description: Option<String>) -> Self {
        match SchemaType::try_get_primitive_type_from_type_id(t.id()) {
            Some(basic_type) => {
                let (minimum, maximum) = t.id().get_min_max_reflect();
                Self {
                    r#type: Some(basic_type),
                    ref_type: None,
                    minimum,
                    maximum,
                    description,
                    ..Default::default()
                }
            }
            None => Self {
                ref_type: Some(TypeReferencePath::definition(t)),
                description,
                ..Default::default()
            },
        }
    }

    pub fn get_type_from_optional(info: &TypeInfo) -> Option<Type> {
        let enum_info = info.as_enum().ok()?;
        if enum_info.generics().len() != 1 {
            return None;
        }
        if !enum_info.contains_variant("None")
            || !enum_info.contains_variant("Some")
            || enum_info.variant_len() != 2
        {
            return None;
        }
        let generic_info = enum_info.generics().first()?;
        Some(*generic_info.ty())
    }

    fn try_get_array_info(info: &TypeInfo) -> Option<Self> {
        match info {
            TypeInfo::List(info) => Some((info.item_info(), info.item_ty()).into()),
            TypeInfo::Set(set_info) => Some(Self::from_type(&set_info.value_ty(), None)),
            TypeInfo::Array(info) => Some((info.item_info(), info.item_ty()).into()),
            _ => None,
        }
    }
}

impl From<&Type> for JsonSchemaBasic {
    fn from(value: &Type) -> Self {
        Self::from_type(value, None)
    }
}

impl From<&TypeInfo> for JsonSchemaBasic {
    fn from(value: &TypeInfo) -> Self {
        JsonSchemaBasic::from_type_info(value, value.to_description())
    }
}

impl From<&NamedField> for JsonSchemaBasic {
    fn from(value: &NamedField) -> Self {
        match value.type_info() {
            Some(info) => JsonSchemaBasic::from_type_info(info, value.to_description()),
            None => value.ty().into(),
        }
    }
}

impl From<&UnnamedField> for JsonSchemaBasic {
    fn from(value: &UnnamedField) -> Self {
        match value.type_info() {
            Some(info) => JsonSchemaBasic::from_type_info(info, value.to_description()),
            None => value.ty().into(),
        }
    }
}

impl From<(Option<&TypeInfo>, Type)> for JsonSchemaBasic {
    fn from((info, ty): (Option<&TypeInfo>, Type)) -> Self {
        match info {
            Some(info) => JsonSchemaBasic::from_type_info(info, info.to_description()),
            None => JsonSchemaBasic::build(&ty),
        }
    }
}

pub trait BasicTypeInfoBuilder {
    fn build_json_schema(&self, t: TypeId) -> Option<JsonSchemaBasic>;
    fn build_json_schema_from_type_path(&self, type_path: &str) -> Option<JsonSchemaBasic>;
    fn add_definitions(&self, basic_info: &JsonSchemaBasic) -> JsonSchemaBasic;
    fn build_json_schema_from_reg(&self, type_reg: &TypeRegistration) -> JsonSchemaBasic;
}

impl BasicTypeInfoBuilder for &TypeRegistry {
    fn build_json_schema_from_type_path(&self, type_path: &str) -> Option<JsonSchemaBasic> {
        let type_reg = self.get_with_type_path(type_path)?;
        Some(self.build_json_schema_from_reg(type_reg))
    }
    fn build_json_schema(&self, type_id: TypeId) -> Option<JsonSchemaBasic> {
        let type_reg = self.get(type_id)?;
        Some(self.build_json_schema_from_reg(type_reg))
    }
    fn build_json_schema_from_reg(&self, type_reg: &TypeRegistration) -> JsonSchemaBasic {
        let mut basic_info = JsonSchemaBasic {
            schema: Some(SchemaMarker),
            description: type_reg.type_info().to_description(),
            r#type: Some(SchemaType::Object),
            ..Default::default()
        };
        if type_reg.data::<ReflectSerializeAsArray>().is_some() {
            if let Ok(struct_info) = type_reg.type_info().as_struct() {
                basic_info.set_type(SchemaType::Array);
                let items: Vec<Box<JsonSchemaBasic>> = struct_info
                    .iter()
                    .map(|field| {
                        let mut value = JsonSchemaBasic::build(field);
                        value.description = Some(field.name().to_string());
                        Box::new(value)
                    })
                    .collect();
                let length = items.len();
                let same_items = items
                    .first()
                    .and_then(|first| Some(items.iter().all(|x| x == first)))
                    .unwrap_or_default();
                if same_items {
                    basic_info.items.push(items[0].clone());
                } else {
                    basic_info.prefix_items = items;
                }
                basic_info.min_items = length.into();
                basic_info.max_items = length.into();
                return basic_info;
            }
        }

        match type_reg.type_info() {
            TypeInfo::Struct(struct_info) => {
                basic_info.set_type(SchemaType::Object);
                basic_info.additional_properties = Some(false);
                basic_info.set_properties(struct_info.iter());
            }
            TypeInfo::TupleStruct(info) => {
                // THIS is for cases like `struct Foo(i32);`.
                if info.field_len() == 1 {
                    let field = info.field_at(0).expect("SHOULD NOT HAPPENED");
                    let field_schema = field.into();
                    return JsonSchemaBasic {
                        description: basic_info.description,
                        ..field_schema
                    };
                }
                basic_info.set_type(SchemaType::Array);
                let length = Some(info.field_len());
                basic_info.set_fixed_array(info.iter(), length, length);
            }
            TypeInfo::Tuple(info) => {
                basic_info.set_type(SchemaType::Array);
                basic_info.set_fixed_array(
                    info.iter(),
                    Some(info.field_len()),
                    Some(info.field_len()),
                );
            }
            TypeInfo::List(info) => {
                basic_info.set_type(SchemaType::Array);
                basic_info.items.push(Box::new(JsonSchemaBasic::build((
                    info.item_info(),
                    info.item_ty(),
                ))));
            }
            TypeInfo::Array(info) => {
                basic_info.set_type(SchemaType::Array);
                basic_info.items.push(Box::new(JsonSchemaBasic::build((
                    info.item_info(),
                    info.item_ty(),
                ))));
                basic_info.max_items = Some(info.capacity());
            }
            TypeInfo::Map(_map_info) => {}
            TypeInfo::Set(info) => {
                basic_info.set_type(SchemaType::Array);
                basic_info
                    .items
                    .push(Box::new(JsonSchemaBasic::build(&info.value_ty())));
            }
            TypeInfo::Enum(info) => {
                // Since we want to use oneOf and some of the variants may not have same type.
                basic_info.r#type = None;
                basic_info.one_of = info
                    .iter()
                    .map(|variant| {
                        let mut schema = JsonSchemaBasic {
                            description: variant.to_description(),
                            ..Default::default()
                        };
                        match variant {
                            VariantInfo::Struct(info) => {
                                schema.set_type(SchemaType::Object);
                                schema.required.push(info.name().to_string());
                                let mut field_schema = JsonSchemaBasic {
                                    r#type: Some(SchemaType::Object),
                                    ..Default::default()
                                };
                                field_schema.set_properties(info.iter());

                                schema
                                    .properties
                                    .insert(info.name().to_string(), Box::new(field_schema));
                            }
                            VariantInfo::Tuple(info) => {
                                schema.set_type(SchemaType::Object);
                                // THIS is for cases like `struct Foo(i32);`.
                                let field_schema = if info.field_len() == 1 {
                                    let field = info.field_at(0).expect("SHOULD NOT HAPPENED");
                                    field.into()
                                } else {
                                    let mut field_schema = JsonSchemaBasic {
                                        r#type: Some(SchemaType::Array),
                                        ..Default::default()
                                    };
                                    let length = Some(info.field_len());
                                    field_schema.set_fixed_array(info.iter(), length, length);
                                    field_schema
                                };
                                schema
                                    .properties
                                    .insert(info.name().to_string(), Box::new(field_schema));
                                schema.required.push(info.name().to_string());
                            }
                            VariantInfo::Unit(unit_variant_info) => {
                                return JsonSchemaVariant::const_value(
                                    unit_variant_info.name().to_string(),
                                );
                            }
                        }

                        JsonSchemaVariant::Schema(Box::new(schema))
                    })
                    .collect();
            }
            TypeInfo::Opaque(info) => {
                basic_info =
                    JsonSchemaBasic::from_type(info.ty(), type_reg.type_info().to_description());
                basic_info.schema = Some(SchemaMarker);
            }
        }
        basic_info
    }
    fn add_definitions(&self, basic_info: &JsonSchemaBasic) -> JsonSchemaBasic {
        let referenced_types = basic_info.get_referenced_types();
        if basic_info.definitions.len() == basic_info.get_referenced_types().len() {
            return basic_info.clone();
        }
        let mut result = basic_info.clone();
        let existing_definitions: Vec<&String> = basic_info.definitions.keys().collect();
        let missing_defs_keys: Vec<TypeReferencePath> = referenced_types
            .iter()
            .flat_map(|key| {
                if existing_definitions.iter().any(|t| t.eq(&&key.0)) {
                    None
                } else {
                    Some(key.clone())
                }
            })
            .collect();
        for missing_def_key in missing_defs_keys {
            let Some(type_reg) = self.get_with_type_path(&missing_def_key.type_path()) else {
                continue;
            };
            let missing_def = self.build_json_schema_from_reg(type_reg);
            result
                .definitions
                .insert(missing_def_key.0, Box::new(missing_def));
        }

        self.add_definitions(&result)
    }
}

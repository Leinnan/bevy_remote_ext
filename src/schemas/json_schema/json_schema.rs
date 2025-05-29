//! JSON Schema Draft 2020-12 types
//! [JSON Schema Draft 2020-12](https://json-schema.org/draft/2020-12/schema)

use bevy_derive::Deref;
use bevy_platform::collections::{HashMap, HashSet};
use bevy_reflect::{
    FromType, NamedField, Reflect, Type, TypeInfo, TypeRegistration, TypeRegistry, UnnamedField,
    VariantInfo, prelude::ReflectDefault, serde::ReflectSerializer,
};
use bevy_utils::default;
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

use super::reflect_helper::{MinMaxTypeReflectHelper, ReflectDocReader};

/// Provides methods for customizing JSON Schema generation for a type
pub trait JsonSchemaProvider {
    /// Returns a custom reference path for this type in the JSON Schema
    /// If None is returned, the default path will be used
    fn get_ref_path() -> Option<TypeReferencePath> {
        None
    }

    /// Returns a custom JSON Schema for this type
    /// If None is returned, the schema will be generated automatically
    fn get_custom_schema() -> Option<JsonSchemaBasic> {
        None
    }
}

/// Reflect-compatible wrapper for JsonSchemaProvider functionality
#[derive(Clone)]
pub struct ReflectJsonSchemaProvider {
    /// Function that returns a custom reference path for a type
    pub get_ref_path: fn() -> Option<TypeReferencePath>,

    /// Function that returns a custom JSON Schema for a type
    pub get_custom_schema: fn() -> Option<JsonSchemaBasic>,
}

impl<T: Reflect + JsonSchemaProvider> FromType<T> for ReflectJsonSchemaProvider {
    fn from_type() -> Self {
        Self {
            get_ref_path: || T::get_ref_path(),
            get_custom_schema: || T::get_custom_schema(),
        }
    }
}

impl FromType<glam::Vec3> for ReflectJsonSchemaProvider {
    fn from_type() -> Self {
        Self {
            get_ref_path: || None,
            get_custom_schema: || {
                Some(JsonSchemaBasic {
                    r#type: Some(SchemaType::Array),
                    max_items: Some(3),
                    min_items: Some(3),
                    prefix_items: vec![
                        Box::new(JsonSchemaBasic {
                            r#type: Some(SchemaType::Number),
                            description: Some("x".to_string()),
                            ..Default::default()
                        }),
                        Box::new(JsonSchemaBasic {
                            r#type: Some(SchemaType::Number),
                            description: Some("y".to_string()),
                            ..Default::default()
                        }),
                        Box::new(JsonSchemaBasic {
                            r#type: Some(SchemaType::Number),
                            description: Some("z".to_string()),
                            ..Default::default()
                        }),
                    ],
                    ..default()
                })
            },
        }
    }
}

/// Type of json schema
/// More [here](https://json-schema.org/draft-07/draft-handrews-json-schema-01#rfc.section.4.2.1)
#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Reflect)]
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
/// Stores information about the location of a reference in a JSON schema.
pub enum ReferenceLocation {
    #[default]
    /// used by json schema draft 7
    Definitions,
    /// used by OpenRPC
    Components,
    /// used by schemas
    Url,
}

impl Display for ReferenceLocation {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ReferenceLocation::Definitions => write!(f, "#/$defs/"),
            ReferenceLocation::Components => write!(f, "#/components/"),
            ReferenceLocation::Url => write!(f, "https://"),
        }
    }
}

/// Stores information about the location and id of a reference in a JSON schema.
#[derive(Debug, Clone, PartialEq, Default, Reflect, Hash, Eq, Ord, PartialOrd)]
pub struct TypeReferencePath {
    /// The location of the reference in the JSON schema.
    pub localization: ReferenceLocation,
    /// The id of the reference.
    pub id: TypeReferenceId,
}

impl TypeReferencePath {
    pub fn is_local(&self) -> bool {
        self.localization == ReferenceLocation::Definitions
            || self.localization == ReferenceLocation::Components
    }
}

#[derive(
    Debug,
    Clone,
    PartialEq,
    Default,
    Reflect,
    Deref,
    Hash,
    Eq,
    Ord,
    PartialOrd,
    Serialize,
    Deserialize,
)]
pub struct TypeReferenceId(String);

impl Display for TypeReferenceId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl TypeReferenceId {
    /// Returns the type path of the reference.
    pub fn type_path(&self) -> String {
        self.replace("-", "::")
    }
}

impl From<&Type> for TypeReferenceId {
    fn from(t: &Type) -> Self {
        TypeReferenceId(t.path().replace("::", "-"))
    }
}
impl From<&str> for TypeReferenceId {
    fn from(t: &str) -> Self {
        TypeReferenceId(t.replace("::", "-"))
    }
}

impl TypeReferencePath {
    /// Creates a new TypeReferencePath with the given type path at the Definitions location.
    pub fn definition(id: impl Into<TypeReferenceId>) -> Self {
        TypeReferencePath::new_ref(ReferenceLocation::Definitions, id)
    }
    /// Creates a new TypeReferencePath with the given location and type path.
    pub fn new_ref<I: Into<TypeReferenceId>>(localization: ReferenceLocation, id: I) -> Self {
        TypeReferencePath {
            localization,
            id: id.into(),
        }
    }

    /// Returns the type path of the reference.
    pub fn type_path(&self) -> String {
        self.id.replace("-", "::")
    }

    pub fn change_localization(&mut self, new_localization: ReferenceLocation) {
        if self.localization.eq(&ReferenceLocation::Url) {
            return;
        }
        self.localization = new_localization;
    }
}
impl Display for TypeReferencePath {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.localization, self.id)
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
        if let Some(definition) = value.strip_prefix(&ReferenceLocation::Definitions.to_string()) {
            Ok(TypeReferencePath::new_ref(
                ReferenceLocation::Definitions,
                definition,
            ))
        } else if let Some(component) =
            value.strip_prefix(&ReferenceLocation::Components.to_string())
        {
            Ok(TypeReferencePath::new_ref(
                ReferenceLocation::Components,
                component,
            ))
        } else if let Some(component) = value.strip_prefix(&ReferenceLocation::Url.to_string()) {
            Ok(TypeReferencePath::new_ref(
                ReferenceLocation::Url,
                component,
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
    BoolValue(bool),
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

#[derive(Debug, Serialize, Deserialize, Clone, Copy, PartialEq, Reflect)]
#[serde(untagged)]
pub enum SchemaNumber {
    PosInt(u64),
    /// Always less than zero.
    NegInt(i64),
    /// Always finite.
    Float(f64),
}

#[derive(Deserialize, Serialize, Debug, Reflect, PartialEq, Clone)]
pub struct SchemaMarker(String);

impl Default for SchemaMarker {
    fn default() -> Self {
        Self("https://json-schema.org/draft/2020-12/schema".to_string())
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, PartialEq, Default, Reflect)]
#[serde(rename_all = "camelCase")]
pub struct JsonSchemaBasic {
    #[serde(rename = "$schema")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub schema: Option<SchemaMarker>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub r#type: Option<SchemaType>,
    #[serde(rename = "$ref")]
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub ref_type: Option<TypeReferencePath>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub minimum: Option<SchemaNumber>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub maximum: Option<SchemaNumber>,
    /// Type description
    #[serde(skip_serializing_if = "Option::is_none", default)]
    pub description: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none", default)]
    #[reflect(ignore)]
    pub items: Option<Box<JsonSchemaBasic>>,
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
    pub additional_properties: Option<JsonSchemaVariant>,
    /// Enforces a single allowed value for the instance.
    #[serde(rename = "const", skip_serializing_if = "Option::is_none", default)]
    pub const_value: Option<JsonSchemaVariant>,
    #[serde(skip_serializing_if = "HashMap::is_empty", default)]
    #[reflect(ignore)]
    #[serde(rename = "$defs")]
    pub definitions: HashMap<TypeReferenceId, Box<JsonSchemaBasic>>,
    /// Default value for the instance.
    #[serde(rename = "default", skip_serializing_if = "Option::is_none", default)]
    #[reflect(ignore)]
    pub default_value: Option<Value>,
}

pub trait SchemaDefinitionsHelper {
    fn get_missing_definitions(&self) -> HashSet<TypeReferencePath> {
        let referenced_types = self.get_referenced_types();
        let definitions = self.get_definitions();
        referenced_types
            .iter()
            .filter(|reference| reference.is_local() && !definitions.contains_key(&reference.id))
            .cloned()
            .collect()
    }
    fn get_referenced_types(&self) -> HashSet<TypeReferencePath>;
    fn get_definitions(&self) -> &HashMap<TypeReferenceId, Box<JsonSchemaBasic>>;
    fn add_definitions(&mut self, definitions: HashMap<TypeReferenceId, Box<JsonSchemaBasic>>);
}

impl SchemaDefinitionsHelper for JsonSchemaBasic {
    fn get_referenced_types(&self) -> HashSet<TypeReferencePath> {
        let mut types = HashSet::new();
        if let Some(ref_type) = &self.ref_type {
            types.insert(ref_type.clone());
        }
        for prefix_item in &self.prefix_items {
            types.extend(prefix_item.get_referenced_types());
        }
        if let Some(item) = &self.items {
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

    fn get_definitions(&self) -> &HashMap<TypeReferenceId, Box<JsonSchemaBasic>> {
        &self.definitions
    }

    fn add_definitions(&mut self, definitions: HashMap<TypeReferenceId, Box<JsonSchemaBasic>>) {
        self.definitions.extend(definitions);
    }
}

impl JsonSchemaBasic {
    pub fn add_property(
        &mut self,
        name: impl Into<String>,
        value: JsonSchemaBasic,
        required: bool,
    ) {
        let name = name.into();
        if required {
            self.required.push(name.clone());
        }
        self.properties.insert(name, Box::new(value));
    }
    pub fn change_referenced_types_location(&mut self, location: ReferenceLocation) {
        if let Some(ref_type) = &mut self.ref_type {
            ref_type.change_localization(location);
        }
        for prefix_item in &mut self.prefix_items {
            prefix_item.change_referenced_types_location(location);
        }
        if let Some(item) = &mut self.items {
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
        self.items = None;
        self.prefix_items.clear();

        // THIS is for cases like `struct Foo(i32);`.
        if fields.len() == 1 {
            let field = fields.last().unwrap();
            let field_schema = field.into();
            *self = JsonSchemaBasic {
                description: self.description.clone(),
                ..field_schema
            };
            return;
        } else {
            self.set_items_fields(fields.map(JsonSchemaBasic::build).collect::<Vec<_>>());
            self.max_items = max_items;
            self.min_items = min_items;
        }
    }

    pub fn set_items_fields(&mut self, items: Vec<JsonSchemaBasic>) {
        // makes sense when all items are the same
        let only_item = if items.len() > 1 {
            let first = items.first().expect("");
            if items.iter().all(|item| item == first) {
                Some(first)
            } else {
                None
            }
        } else {
            items.first()
        };
        self.set_type(SchemaType::Array);
        if let Some(only_item) = only_item {
            self.prefix_items.clear();
            self.items = Some(Box::new(only_item.clone()));
        } else {
            self.items = None;
            self.prefix_items = items.iter().map(|item| Box::new(item.clone())).collect();
        }
    }

    pub fn set_properties(&mut self, fields: core::slice::Iter<'_, NamedField>) {
        self.properties.clear();
        self.required.clear();
        self.set_type(SchemaType::Object);
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
        if let Ok(map_info) = t.as_map() {
            if map_info.key_ty().id().eq(&TypeId::of::<String>()) {
                info.set_type(SchemaType::Object);
                info.additional_properties = Some(JsonSchemaVariant::Schema(Box::new(
                    JsonSchemaBasic::build((map_info.value_info(), map_info.value_ty())),
                )));
            }
        }
        if let Some(array_data) = Self::try_get_array_info(t) {
            info.set_type(SchemaType::Array);
            info.min_items = array_data.min_items;
            info.max_items = array_data.max_items;
            info.items = Some(Box::new(array_data.items));
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

    fn try_get_array_info(info: &TypeInfo) -> Option<ArrayInfo> {
        match info {
            TypeInfo::List(info) => Some(ArrayInfo {
                min_items: None,
                max_items: None,
                items: (info.item_info(), info.item_ty()).into(),
            }),
            TypeInfo::Set(set_info) => Some(ArrayInfo {
                min_items: None,
                max_items: None,
                items: Self::from_type(&set_info.value_ty(), None),
            }),
            TypeInfo::Array(info) => Some(ArrayInfo {
                min_items: Some(info.capacity()),
                max_items: Some(info.capacity()),
                items: (info.item_info(), info.item_ty()).into(),
            }),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayInfo {
    pub min_items: Option<usize>,
    pub max_items: Option<usize>,
    pub items: JsonSchemaBasic,
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
    fn build_with_definitions<T: SchemaDefinitionsHelper + Clone>(&self, basic_info: &T) -> T;
    fn build_json_schema_from_reg(&self, type_reg: &TypeRegistration) -> JsonSchemaBasic;
}

impl BasicTypeInfoBuilder for TypeRegistry {
    fn build_json_schema_from_type_path(&self, type_path: &str) -> Option<JsonSchemaBasic> {
        let type_reg = self.get_with_type_path(type_path)?;
        Some(self.build_json_schema_from_reg(type_reg))
    }
    fn build_json_schema(&self, type_id: TypeId) -> Option<JsonSchemaBasic> {
        let type_reg = self.get(type_id)?;
        Some(self.build_json_schema_from_reg(type_reg))
    }
    fn build_json_schema_from_reg(&self, type_reg: &TypeRegistration) -> JsonSchemaBasic {
        if let Some(schema_provider) = type_reg.data::<ReflectJsonSchemaProvider>() {
            if let Some(custom_schema) = (schema_provider.get_custom_schema)() {
                return custom_schema;
            }
            if let Some(ref_path) = (schema_provider.get_ref_path)() {
                return JsonSchemaBasic {
                    ref_type: Some(ref_path),
                    description: type_reg.type_info().to_description(),
                    ..default()
                };
            }
        }
        let mut basic_info = JsonSchemaBasic {
            schema: Some(SchemaMarker::default()),
            description: type_reg.type_info().to_description(),
            r#type: Some(SchemaType::Object),
            ..Default::default()
        };
        if let Some(data) = self.get_type_data::<ReflectDefault>(type_reg.type_id()) {
            let default = data.default();
            let serializer = ReflectSerializer::new(&*default, self);
            if let Some(value_object) = serde_json::to_value(serializer)
                .ok()
                .and_then(|v| v.as_object().cloned())
            {
                if value_object.len() == 1 {
                    if let Some((_, value)) = value_object.into_iter().next() {
                        basic_info.default_value = Some(value);
                    }
                }
            }
        }

        match type_reg.type_info() {
            TypeInfo::Struct(struct_info) => {
                basic_info.additional_properties = Some(JsonSchemaVariant::BoolValue(false));
                basic_info.set_properties(struct_info.iter());
            }
            TypeInfo::TupleStruct(info) => {
                let length = Some(info.field_len());
                basic_info.set_fixed_array(info.iter(), length, length);
            }
            TypeInfo::Tuple(info) => {
                basic_info.set_fixed_array(
                    info.iter(),
                    Some(info.field_len()),
                    Some(info.field_len()),
                );
            }
            TypeInfo::List(info) => {
                basic_info.set_items_fields(vec![JsonSchemaBasic::build((
                    info.item_info(),
                    info.item_ty(),
                ))]);
            }
            TypeInfo::Array(info) => {
                basic_info.set_items_fields(vec![JsonSchemaBasic::build((
                    info.item_info(),
                    info.item_ty(),
                ))]);
                basic_info.min_items = Some(info.capacity());
                basic_info.max_items = Some(info.capacity());
            }
            TypeInfo::Map(map_info) => {
                if map_info.key_ty().id().eq(&TypeId::of::<String>()) {
                    basic_info.set_type(SchemaType::Object);
                    basic_info.additional_properties = Some(JsonSchemaVariant::Schema(Box::new(
                        JsonSchemaBasic::build((map_info.value_info(), map_info.value_ty())),
                    )));
                }
            }
            TypeInfo::Set(info) => {
                basic_info.set_type(SchemaType::Array);
                basic_info.items = Some(Box::new(JsonSchemaBasic::build(&info.value_ty())));
            }
            TypeInfo::Enum(info) => {
                // Since we want to use oneOf and some of the variants may not have same type.
                basic_info.r#type = None;
                basic_info.one_of = info
                    .iter()
                    .map(|variant| {
                        let property = match variant {
                            VariantInfo::Struct(info) => {
                                let mut field_schema = JsonSchemaBasic {
                                    r#type: Some(SchemaType::Object),
                                    ..Default::default()
                                };
                                field_schema.set_properties(info.iter());
                                field_schema
                            }
                            VariantInfo::Tuple(info) => {
                                info.iter().build_json_schema(info.to_description())
                            }
                            VariantInfo::Unit(unit_variant_info) => {
                                return JsonSchemaVariant::const_value(
                                    unit_variant_info.name().to_string(),
                                );
                            }
                        };
                        let mut schema = JsonSchemaBasic {
                            description: variant.to_description(),
                            r#type: Some(SchemaType::Object),
                            ..Default::default()
                        };
                        schema.add_property(variant.name(), property, true);

                        JsonSchemaVariant::Schema(Box::new(schema))
                    })
                    .collect();
            }
            TypeInfo::Opaque(info) => {
                basic_info =
                    JsonSchemaBasic::from_type(info.ty(), type_reg.type_info().to_description());
                basic_info.schema = Some(SchemaMarker::default());
            }
        }
        basic_info.description = type_reg.type_info().to_description();
        basic_info
    }
    fn build_with_definitions<T: SchemaDefinitionsHelper + Clone>(&self, basic_info: &T) -> T {
        let missing_definitions = basic_info.get_missing_definitions();
        let mut result = basic_info.clone();
        if missing_definitions.is_empty() {
            return result;
        }
        result.add_definitions(
            missing_definitions
                .iter()
                .flat_map(|def| {
                    let Some(type_reg) = self.get_with_type_path(&def.type_path()) else {
                        return None;
                    };
                    Some((
                        def.id.clone(),
                        Box::new(self.build_json_schema_from_reg(type_reg)),
                    ))
                })
                .collect::<HashMap<TypeReferenceId, Box<JsonSchemaBasic>>>(),
        );

        self.build_with_definitions(&result)
    }
}

pub trait JsonSchemaBuilder {
    fn build_json_schema(&self, documentation: Option<String>) -> JsonSchemaBasic {
        let mut basic_info = JsonSchemaBasic::default();
        basic_info.schema = Some(SchemaMarker::default());
        basic_info.description = documentation;
        self.feed_data(&mut basic_info);
        basic_info
    }
    fn feed_data(&self, data: &mut JsonSchemaBasic);
}

impl JsonSchemaBuilder for core::slice::Iter<'_, UnnamedField> {
    fn feed_data(&self, data: &mut JsonSchemaBasic) {
        let length = Some(self.len());
        data.set_fixed_array(self.clone(), length, length);
    }
}

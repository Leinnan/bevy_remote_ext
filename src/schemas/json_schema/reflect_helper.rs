//! Helper functions for working with JSON Schema and reflection.

use std::any::TypeId;

use bevy_reflect::{
    GetTypeRegistration, NamedField, Reflect, SetInfo, TupleVariantInfo, TypeInfo, TypePath,
    UnnamedField, VariantInfo,
};

use super::json_schema::SchemaNumber;

/// Trait for reading documentation from reflection data.
pub trait ReflectDocReader {
    /// Builds the description based on the reflected data.
    fn to_description(&self) -> Option<String> {
        self.get_docs()
            .map(|s| s.trim().replace("\n", "").to_string())
    }

    /// Returns the documentation of the reflected data.
    fn get_docs(&self) -> Option<&str> {
        None
    }
}

impl ReflectDocReader for VariantInfo {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        match self {
            VariantInfo::Unit(info) => info.docs(),
            VariantInfo::Tuple(info) => info.docs(),
            VariantInfo::Struct(info) => info.docs(),
        }
    }
}

impl ReflectDocReader for TypeInfo {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        self.docs()
    }
}

impl ReflectDocReader for NamedField {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        if self.docs().is_none() {
            self.type_info().and_then(|f| f.docs())
        } else {
            self.docs()
        }
    }
}
impl ReflectDocReader for UnnamedField {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        if self.docs().is_none() {
            self.type_info().and_then(|f| f.docs())
        } else {
            self.docs()
        }
    }
}

impl ReflectDocReader for SetInfo {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        self.docs()
    }
}

impl ReflectDocReader for TupleVariantInfo {
    #[cfg(feature = "documentation")]
    fn get_docs(&self) -> Option<&str> {
        self.docs()
    }
}

#[derive(Clone, Debug, Default)]
pub struct SchemaExtraInfo {
    pub documentation: Option<String>,
    pub min_value: Option<SchemaNumber>,
    pub max_value: Option<SchemaNumber>,
}

impl SchemaExtraInfo {
    pub fn just_docs<T: ReflectDocReader>(ty: &T) -> Self {
        SchemaExtraInfo{
            documentation: ty.to_description(),
            ..Default::default()
        }
    }
    pub fn docs_with_type<T: ReflectDocReader>(ty: &T, type_id: &TypeId) -> Self {        
        let (min_value, max_value) = type_id.get_min_max_reflect();
        SchemaExtraInfo{
            documentation: ty.to_description(),
            min_value,
            max_value
        }
    }
}

impl From<&UnnamedField> for SchemaExtraInfo {
    fn from(field: &UnnamedField) -> Self {
        let (min_value, max_value) = field.type_id().get_min_max_reflect();
        SchemaExtraInfo{
            documentation: field.to_description(),
            min_value,
            max_value
        }
    }
}

impl From<&NamedField> for SchemaExtraInfo {
    fn from(field: &NamedField) -> Self {
        let (min_value, max_value) = field.type_id().get_min_max_reflect();
        SchemaExtraInfo{
            documentation: field.to_description(),
            min_value,
            max_value
        }
    }
}

impl From<&TypeInfo> for SchemaExtraInfo {
    fn from(type_info: &TypeInfo) -> Self {
        let (min_value, max_value) = type_info.type_id().get_min_max_reflect();
        SchemaExtraInfo{
            documentation: type_info.to_description(),
            min_value,
            max_value
        }
    }
}

impl From<TypeId> for SchemaExtraInfo {
    fn from(field: TypeId) -> Self {
        let mut info = SchemaExtraInfo::default();
        let (min, max) = field.get_min_max_reflect();
        info.min_value = min;
        info.max_value = max;
        info
    }
}

pub trait MinMaxTypeReflectHelper {
    /// Get the underlaying TypeId
    fn get_type(&self) -> TypeId;

    /// Compares if is the same type
    fn is_type<T: Sized + 'static>(&self) -> bool {
        self.get_type().eq(&TypeId::of::<T>())
    }

    /// Tries to get attribute data 
    fn get_data<T: Sized + 'static + GetTypeRegistration + Reflect + TypePath + Clone>(
            &self
        ) -> Option<&core::ops::RangeInclusive<T>> {
            if !self.is_type::<T>(){
                None
            } else {
                self.try_get_range::<T>()
            }
        }
    

    fn try_get_range<T: Sized + 'static + GetTypeRegistration + Reflect + TypePath + Clone>(&self) -> Option<&core::ops::RangeInclusive<T>>;

    /// Get the minimum and maximum values for a given type.
    fn get_min_max_reflect(&self) -> (Option<SchemaNumber>, Option<SchemaNumber>){
        let (mut min, mut max) = self.get_base_min_max_for_type();
        if let Some(data) = self.get_data::<u8>() {
            min = Some(SchemaNumber::PosInt(*data.start() as u64));
            max = Some(SchemaNumber::PosInt(*data.end() as u64));
        }
        if let Some(data) = self.get_data::<u16>() {
            min = Some(SchemaNumber::PosInt(*data.start() as u64));
            max = Some(SchemaNumber::PosInt(*data.end() as u64));
        }
        if let Some(data) = self.get_data::<u32>() {
            min = Some(SchemaNumber::PosInt(*data.start() as u64));
            max = Some(SchemaNumber::PosInt(*data.end() as u64));
        }
        if let Some(data) = self.get_data::<u64>() {
            min = Some(SchemaNumber::PosInt(*data.start()));
            max = Some(SchemaNumber::PosInt(*data.end()));
        }

        if let Some(data) = self.get_data::<i8>() {
            min = Some(SchemaNumber::NegInt((*data.start()).into()));
            max = Some(SchemaNumber::NegInt((*data.end()).into()));
        }
        if let Some(data) = self.get_data::<i16>() {
            min = Some(SchemaNumber::NegInt((*data.start()).into()));
            max = Some(SchemaNumber::NegInt((*data.end()).into()));
        }
        if let Some(data) = self.get_data::<i32>() {
            min = Some(SchemaNumber::NegInt((*data.start()).into()));
            max = Some(SchemaNumber::NegInt((*data.end()).into()));
        }
        if let Some(data) = self.get_data::<i64>() {
            min = Some(SchemaNumber::NegInt(*data.start()));
            max = Some(SchemaNumber::NegInt(*data.end()));
        }
        (min, max)
    }

    fn get_base_min_max_for_type(&self) -> (Option<SchemaNumber>, Option<SchemaNumber>){
        let mut min: Option<SchemaNumber> = None;
        let mut max: Option<SchemaNumber> = None;
        if self.is_type::<u8>() {
            min = Some(SchemaNumber::PosInt(0));
            max = Some(SchemaNumber::PosInt(u8::MAX.into()));
        }
        if self.is_type::<u16>() {
            min = Some(SchemaNumber::PosInt(0));
            max = Some(SchemaNumber::PosInt(u16::MAX.into()));
        }
        if self.is_type::<u32>() {
            min = Some(SchemaNumber::PosInt(0));
            max = Some(SchemaNumber::PosInt(u32::MAX.into()));
        }
        if self.is_type::<u64>() {
            min = Some(SchemaNumber::PosInt(0));
        }
        if self.is_type::<u128>() {
            min = Some(SchemaNumber::PosInt(0));
        }
        if self.is_type::<usize>() {
            min = Some(SchemaNumber::PosInt(0));
        }
        if self.is_type::<i8>() {
            min = Some(SchemaNumber::NegInt(i8::MIN.into()));
            max = Some(SchemaNumber::NegInt(i8::MAX.into()));
        }
        if self.is_type::<i16>() {
            min = Some(SchemaNumber::NegInt(i16::MIN.into()));
            max = Some(SchemaNumber::NegInt(i16::MAX.into()));
        }
        if self.is_type::<i32>() {
            min = Some(SchemaNumber::NegInt(i32::MIN.into()));
            max = Some(SchemaNumber::NegInt(i32::MAX.into()));
        }
        (min, max)
    }
}
impl MinMaxTypeReflectHelper for UnnamedField {
    
    fn get_type(&self) -> TypeId {
        self.type_id()
    }
    
    fn try_get_range<T: Sized + 'static + GetTypeRegistration + Reflect + TypePath + Clone>(&self) -> Option<&core::ops::RangeInclusive<T>> {
        self.get_attribute::<core::ops::RangeInclusive<T>>()
    }
}

impl MinMaxTypeReflectHelper for NamedField {
    fn get_type(&self) -> TypeId {
        self.type_id()
    }
    
    fn try_get_range<T: Sized + 'static + GetTypeRegistration + Reflect + TypePath + Clone>(&self) -> Option<&core::ops::RangeInclusive<T>> {
        self.get_attribute::<core::ops::RangeInclusive<T>>()
    }
}

impl MinMaxTypeReflectHelper for TypeId {
    fn get_type(&self) -> TypeId {
        *self
    }

    fn try_get_range<T: Sized + 'static + GetTypeRegistration + Reflect + TypePath + Clone>(&self) -> Option<&core::ops::RangeInclusive<T>> {
        None
    }
}

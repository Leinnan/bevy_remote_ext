//! Helper functions for working with JSON Schema and reflection.

use std::any::TypeId;

use bevy_reflect::{NamedField, SetInfo, TypeInfo, UnnamedField, VariantInfo};

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

pub trait MinMaxTypeReflectHelper {
    /// Get the minimum and maximum values for a given type.
    fn get_min_max_reflect(&self) -> (Option<i64>, Option<i64>);
}

impl MinMaxTypeReflectHelper for NamedField {
    fn get_min_max_reflect(&self) -> (Option<i64>, Option<i64>) {
        let value = self.type_id();
        fn is_type<T: Sized + 'static>(v: TypeId) -> bool {
            v.eq(&TypeId::of::<T>())
        }

        let mut min: Option<i64> = None;
        let mut max: Option<i64> = None;
        if is_type::<u8>(value) {
            min = Some(0);
            max = Some(u8::MAX.into());
        }
        if is_type::<u16>(value) {
            min = Some(0);
            max = Some(u16::MAX.into());
        }
        if is_type::<u32>(value) {
            min = Some(0);
            max = Some(u32::MAX.into());
        }
        if is_type::<u64>(value) {
            min = Some(0);
        }
        if is_type::<u128>(value) {
            min = Some(0);
        }
        if is_type::<usize>(value) {
            min = Some(0);
        }
        if is_type::<i8>(value) {
            min = Some(i8::MIN.into());
            max = Some(i8::MAX.into());
        }
        if is_type::<i16>(value) {
            min = Some(i16::MIN.into());
            max = Some(i16::MAX.into());
        }
        if is_type::<i32>(value) {
            min = Some(i32::MIN.into());
            max = Some(i32::MAX.into());
        }
        (min, max)
    }
}

impl MinMaxTypeReflectHelper for TypeId {
    fn get_min_max_reflect(&self) -> (Option<i64>, Option<i64>) {
        fn is_type<T: Sized + 'static>(v: &TypeId) -> bool {
            v.eq(&TypeId::of::<T>())
        }

        let mut min: Option<i64> = None;
        let mut max: Option<i64> = None;
        if is_type::<u8>(self) {
            min = Some(0);
            max = Some(u8::MAX.into());
        }
        if is_type::<u16>(self) {
            min = Some(0);
            max = Some(u16::MAX.into());
        }
        if is_type::<u32>(self) {
            min = Some(0);
            max = Some(u32::MAX.into());
        }
        if is_type::<u64>(self) {
            min = Some(0);
        }
        if is_type::<u128>(self) {
            min = Some(0);
        }
        if is_type::<usize>(self) {
            min = Some(0);
        }
        if is_type::<i8>(self) {
            min = Some(i8::MIN.into());
            max = Some(i8::MAX.into());
        }
        if is_type::<i16>(self) {
            min = Some(i16::MIN.into());
            max = Some(i16::MAX.into());
        }
        if is_type::<i32>(self) {
            min = Some(i32::MIN.into());
            max = Some(i32::MAX.into());
        }
        (min, max)
    }
}

# Bevy remote ext

Custom version of bevy_remote package, with focus on better support of JSON Schema draft 7.

End goal is making it better support the standard and being able to use schemas in various IDEs for content validation.


## Using

If you have asset that are reflectable Rust type, you can call `export_type_json_schema_with_definitions` for that type and save the result in file and specify in IDE.

Like here: [VSCode](https://code.visualstudio.com/docs/languages/json#_mapping-to-a-schema-in-the-workspace)

## Serialize as Array reflect info

Some libraries are implementing custom serialization, like glam for various types. While I did not found the way to detect that, in this version we can add extra information about the type to the type registry.
Then it can be used like that : `app.register_type_data::<glam::Vec3, ReflectSerializeAsArray>();`.

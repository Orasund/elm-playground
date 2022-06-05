module {{moduleBase}}.{{template}}.{{moduleName}} exposing (..)

{-| This module contains the {{moduleName}} {{template}}.

@docs {{moduleName}}

## Is

@docs {{#variants}}{{#if @first}}{{else}} ,{{/if}}is{{name}}{{/variants}}

## If

@docs {{#variants}}{{#if @first}}{{else}} ,{{/if}}{{#if (or polymorphic type)}}if{{name}}{{/if}}{{/variants}}

## Map

@docs {{#variants}}{{#if @first}}{{else}} ,{{/if}}{{#if (or polymorphic type)}}map{{name}}{{/if}}{{/variants}}
-}
{{#if imports}}{{#imports}}
import {{.}}
{{/imports}}{{/if}}

-- This is a generated file. DO NOT CHANGE ANYTHING IN HERE.

{-| {{moduleName}} type
-}
type {{moduleName}}{{#variants}}{{#if polymorphic}} a{{@index}}{{/if}}{{/variants}}
{{#variants}}
{{#if @first}}   = {{name}}{{else}}   | {{name}}{{/if}} {{#if (or polymorphic type)}}({{#if type}}{{type}}{{/if}} {{#if polymorphic}} a{{@index}}{{/if}}){{/if}}
{{/variants}}

-------------------------------------------------------------------------------
-- IS
-------------------------------------------------------------------------------
{{#variants}}
{-| Compute if the value is a {{name}}.

Useful if you don't want to do a full case distinction inside an if condition.

-}
is{{name}} : {{../moduleName}}{{#../variants}}{{#if polymorphic}} a{{@index}}{{/if}}{{/../variants}} -> Bool
is{{name}} {{decapitalize ../moduleName}} = 
    case {{decapitalize ../moduleName}} of
{{#../variants}}
{{#if (or type polymorphic)}}
        {{name}} _ -> {{#if (eq @index @../index)}} True {{else}} False {{/if}}
{{else}}
        {{name}} -> {{#if (eq @index @../index)}} True {{else}} False {{/if}}
{{/if}}
{{/../variants}}
{{/variants}}

-------------------------------------------------------------------------------
-- IF
-------------------------------------------------------------------------------
{{#variants}}
{{#if (or polymorphic type)}}
{-| Calls a function, if the value is a {{name}}.

You can turn the function into a getter by passing the identity as function

-}
if{{name}} : {{#if type}}{{#if polymorphic}}({{type}} {{#../variants}}{{#if (and polymorphic (eq @index @../index))}} a{{@index}}{{/if}}{{/../variants}} -> out){{else}}({{type}} -> out){{/if}}{{else if polymorphic}}({{#../variants}}{{#if (and polymorphic (eq @index @../index))}} a{{@index}}{{/if}}{{/../variants}} -> out){{/if}} -> {{../moduleName}}{{#../variants}}{{#if polymorphic}} a{{@index}}{{/if}}{{/../variants}} -> Maybe out
if{{name}} fun {{decapitalize ../moduleName}} =
    case {{decapitalize ../moduleName}} of
{{#../variants}}
{{#if (eq @index @../index)}}
    {{name}} t -> Just (fun t)
{{else if (or type polymorphic)}}
    {{name}} a -> Nothing
{{else}}
    {{name}} -> Nothing
{{/if}}
{{/../variants}}

{{/if}}
{{/variants}}

-------------------------------------------------------------------------------
-- MAP
-------------------------------------------------------------------------------
{{#variants}}
{{#if (or polymorphic type)}}
{-| Map the argument of {{name}}.
-}
map{{name}} : {{#if type}}{{#if polymorphic}}({{type}} {{#../variants}}{{#if (and polymorphic (eq @index @../index))}} a{{@index}}{{/if}}{{/../variants}} -> {{type}}{{#../variants}}{{#if (and polymorphic (eq @index @../index))}} b{{@index}}{{/if}}{{/../variants}}){{else}}({{type}} -> {{type}}){{/if}}{{else if polymorphic}}({{#../variants}}{{#if (and polymorphic (eq @index @../index))}} a{{@index}}{{/if}}{{/../variants}} -> {{#../variants}}{{#if (and polymorphic (eq @index @../index))}} b{{@index}}{{/if}}{{/../variants}}){{/if}} -> {{../moduleName}}{{#../variants}}{{#if polymorphic}} a{{@index}}{{/if}}{{/../variants}} -> {{../moduleName}}{{#../variants}}{{#if polymorphic}}{{#if (eq @index @../index )}} b{{else}} a{{/if}}{{@index}}{{/if}}{{/../variants}}
map{{name}} fun {{decapitalize ../moduleName}} =
    case {{decapitalize ../moduleName}} of
{{#../variants}}
{{#if (eq @index @../index)}}
    {{name}} t -> {{name}} (fun t)
{{else if (or type polymorphic)}}
    {{name}} a -> {{name}} a
{{else}}
    {{name}} -> {{name}}
{{/if}}
{{/../variants}}
{{/if}}

{{/variants}}

-- Generated with [Elm-pen](https://orasund.github.io/elm-pen) Version {{version}}

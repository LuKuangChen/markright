module Concept = Markright__Concept
module Extension = Markright__Extension
let compile: (string, ~extensions: RescriptCore.Dict.t<Extension.t>=?) => string

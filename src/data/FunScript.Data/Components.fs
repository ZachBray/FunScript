module FunScript.Data.Components

open FunScript.Data

let getDataProviders() =
  ApiaryProvider.getComponents() @
  JsonProvider.getComponents() @
  WorldBank.getComponents()


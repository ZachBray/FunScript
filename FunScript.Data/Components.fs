module FunScript.Data.Components

open FunScript.Data

let DataProviders =
  ApiaryProvider.components @
  JsonProvider.components @
  WorldBank.components


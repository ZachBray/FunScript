# FunScript

**FunScript** consists of a standalone F# to JavaScript compiler library and a [Type Provider](http://msdn.microsoft.com/en-us/library/hh156509.aspx) to [TypeScript](http://typescriptlang.org/).
**FunScript** lets you connect to client and server APIs with [Intellisense](http://en.wikipedia.org/wiki/IntelliSense) and types [automagically](http://www.urbandictionary.com/define.php?term=automagically) using F# 3's Type Provider mechanism.

Status:
* Windows: [![TeamCity build status](http://teamcity.codebetter.com/app/rest/builds/buildType:\(id:bt1106\)/statusIcon)](http://teamcity.codebetter.com/viewType.html?buildTypeId=bt1106) 
* Mono: [![Build Status](https://travis-ci.org/enricosada/FunScript.png)](https://travis-ci.org/enricosada/FunScript)

## Compiler Library

The compiler library generates JavaScript code that can be run in-browser or server-side inside [Node.js](http://nodejs.org/).
Simply mark F# modules to be compiled with the [ReflectedDefinition](http://msdn.microsoft.com/en-us/library/ee353643.aspx) attribute. 
The compiler library has minimal dependencies and can be run inside either Visual Studio or MonoDevelop.

## Contributing

Please help test the compiler libary and type provider by building samples and reporting issues.

## More information

Please see the [FunScript website](http://www.funscript.info).
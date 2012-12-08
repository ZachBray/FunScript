# FunScript

**FunScript** consists of a standalone F# to JavaScript compiler [library](http://en.wikipedia.org/wiki/Library_(computing)) and a [Type Provider](http://msdn.microsoft.com/en-us/library/hh156509.aspx) to [TypeScript](http://typescriptlang.org/).
**FunScript** lets you connect to client and server APIs with [Intellisense](http://en.wikipedia.org/wiki/IntelliSense) and types [automagically](http://www.urbandictionary.com/define.php?term=automagically) using F# 3's Type Provider mechanism.

## Compiler Library

The compiler library generates JavaScript code that can be run in-browser or server-side inside [Node.js](http://nodejs.org/).
Simply mark F# modules to be compiled with the [ReflectedDefinition](http://msdn.microsoft.com/en-us/library/ee353643.aspx) attribute. 
The compiler library has minimal dependencies and can be run inside Visual Studio or MonoDevelop.

## TypeScript Type Provider

The TypeScript Type Provider leverages the excellent work of the TypeScript community to annotate popular JavaScript libraries like [jQuery](http://jquery.com/) with type information by parsing [TypeScript definition files](https://github.com/borisyankov/DefinitelyTyped).

## Presentations

Tomas Petricek demonstrated FunScript at the TechMesh conference in December 2012, in his presentation [How F# Learned to Stop Worrying and Love the Data](http://techmeshconf.com/techmesh-london-2012/presentation/How%20F# Learned to Stop Worrying and Love the Data).
Try the [WorldBank sample](http://tomasp.net/techmesh/worldbank.html) and [Movie Database sample](http://tomasp.net/techmesh/moviedatabase.html) in your browser.

## Contributing

Please help test the compiler libary and type provider by building samples and reporting issues.




Intended as a reference project for beginners who aim to get up and running and learn the basics.
Moreover, intended to focus on examples leveraging FunScript for Mobile development, especially with
JQueryMobile and PhoneGap (TBD).

Used with MS Visual Studio 2013 (professional), 
with the vanilla "FunScript - Not Hosted" project template (by Zach Bary), 
which can be found through the online project search. Plus relevant FunScript binding packages (through NuGet).

Your todo:
REPLACE the "FunScript - Not Hosted" Project.fs file contents with this file.
ADD relevant FunScript.TypeScript.Binding.? packages 
    Through NuGet manager for your solution: FunScript.TypeScript.Binding.jquerymobile and FunScript.TypeScript.Binding.phonegap
ADD the rest of the examples *.fs files to the visual studio project. NOTICE: file order matters for MS VS.
SELECT (comment/uncomment the desired line) inside Program.fs, towards the end, which example page you want to write into the index file.

Available examples (in addition to the main Program.fs module):
1. HtmlCanvasExample - simply show how to inject canvas drawing into an HTML text
2. BodyElementsExample - same example as HtmlCanvasExample , but we construct the whole body with Funscript
3. JQueryMobileExample - showing some Funscript and HTML with jquerymobile
4. Mazing - a maze game example that is written with jquerymobile. The "goat.jpg" is used for the exit indicator ingame.
5. Shakeamaze - [TBD] the maze example with Cordova/Phonegap accelerator api


Leveraging some examples from the Funscript GitHub directory:
https://github.com/ZachBray/FunScript/tree/master/Examples
Such as the Canvas example (by Zach Bary) - offered as a good first project (hello-world alternative) here.

Adding examples for JQueryMobile and PhoneGap (TBD).

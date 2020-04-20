# Wolfram IntelliJ Tools

This package provides functions to collect information about a Wolfram Mathematica system which are later used in
the Wolfram Language IntelliJ Plugin to provide

- autocompletion of built-in functions, options and symbols
- usage messages for quickly looking up the documentation of symbols
- version check to see if you're using functions not available in your Mathematica version
- additional information to provide correct Options for a function or sort autocompletion results by usage frequency
- lexer tokens for named-characters that are used in the Plugin parser
- a dictionary containing Mathematica-specific words so that they don't show up during spell-check

## Usage

Note that **loading the package** and **package functions** will take some time to finish since they are extracting
information from many thousand Mathematica symbols.
To create JSON files used for autocompletion and version checks, you can run the following in
Mathematica 12.1:

```wl
PacletDirectoryLoad["/path/to/Wolfram-IntelliJ-Tools/IntelliJTools/IntelliJTools"]

Needs["IntelliJTools`"]
SaveCompleteMathematicaInformation["/existing/output/directory"]
```

### Creating Usage message help pages

To create HTML pages for usage messages of Mathematica functions, you can run

```wl
CreateAllHtmlUsageMessages["/existing/output/directory"]
```

Note that this code relies on quite some hacks because it tries to convert usage message strings into HTML/MathML and 
preserve basic mathematical formatting. The algorithm works as follows:

1. Create the box representation of the styled usage message strings with `createBoxes`
2. Transform the boxes into HTML/MathML by converting all box-expressions with `box2HTML`
3. Fix the underlying strings to create real UTF characters from Mathematica's named characters if possible with `processString`
4. Put everything together by including a link to the online help, Options, and Attributes with `CreateHTMLUsageString`
5. In `CreateAllHtmlUsageMessages` all symbols from important contexts are processed and stored in single HTML files.

For debugging, check the output of `createBoxes[symName]` and use it with `box2HTML` to ensure the usage message text is converted correctly.



### Creating custom Mathematica dictionary

To create the spell-check dictionary, you can run

```wl
SaveDictionary["/existing/output/directory"]
```

which will put a `WLDictionary.dic` file in the output directory. This function needs revision since it contains
false-positive words that are actually not words. Even in Mathematica.
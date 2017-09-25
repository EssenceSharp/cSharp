# Configuration Profiles
A configuration profile is persistently stored as a folder in the filesystem. It must be located in the folder %EssenceSharpPath%\Config. The folder name must use ".profile" as its file extension. A configuration profile folder may (optionally) contain one or more files--each with its own format--that specify configuration options that globally apply to Essence#.

Currently, a configuration profile can be used to define an assembly map, to define a list of class library search paths and to define a list of script search paths.

An AssemblyMap configuration file must be named "AssemblyMap.es." If present, it must contain an Essence# dictionary literal (which may be empty,) or else the file must be empty. If the dictionary literal isn't empty, the keys in the dictionary must be logical assembly names, and the values must be physical assembly names that can be used to actually load a .Net assembly.

A search paths file must have the file extension ".searchPaths" and must contain pathnames of folders to be searched, one per line with no other punctuation, listed in the desired sequence, from first to last. The listed pathnames must be absolute pathnames, but may use environment variable references, such as %UserProfile% or %EssenceSharpPath%.

The name of the search paths file for class libraries must be "library.searchPaths", and the name of the search paths file for scripts must be "scripts.searchPaths". Neither file is required.

Any number of different configuration profiles may be used at the same time, or even none at all. To specify the set of configuration profiles that should be used, list their names--one per line with no other punctuation--in the "activeProfiles" text file, which resides in the %EssenceSharpPath%\Config folder. It is not necessary to use the ".profile" extension in the list of names.

Note that the order in which configuration profiles are listed in the "activeProfiles" file matters, because the effect of combining two or more of them together is additive, but conflicts between them are resolved by using the configuration options specified by whichever profile occurs later in the list of active configuration profiles.

The current version, and all previous versions, of the standard set of configuration profiles may be obtained from the [Essence# site on GitHub](https://github.com/EssenceSharp/Config).

_
The essence of OOP: It's all messages, all the time._



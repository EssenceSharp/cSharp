# Downloading

You can download the latest version by clicking on the big purple button labeled **DOWNLOAD** on the upper right-hand side of the site's home page. Or, if you'd like to read the release notes (recommended,) you can navigate to the tab labeled **DOWNLOADS** on the tabs bar (near the top of the page; the leftmost tab is labeled **HOME.)** Both options will get you a program that installs all of Essence#, including the Essence# [Standard Library](Standard-Library) and the Microsoft Visual Studio project folder used to develop the C# code that implements Essence# (the compiler and the run time system.) 

You can also obtain components of Essence# on an ad-hoc ("a la carte") basis:

The **SOURCE CODE** tab (above) will let you either download just the Visual Studio project or else make a local clone of the Git repository for it--but neither of those options will get you the Essence# [Standard Library](Standard-Library), which is required to actually use Essence#.

Other components of Essence# are available individually from the [Essence# site on GitHub](https://github.com/EssenceSharp?tab=repositories):

The Essence# [Standard Library repository](https://github.com/EssenceSharp/Standard.lib) on GitHub: Contains the Essence# [Standard Library](Standard-Library)

The Essence# [binaries repository](https://github.com/EssenceSharp/Bin) on GitHub: Contains the binary executables needed to run Essence#

The Essence# [scripts repository](https://github.com/EssenceSharp/Scripts) on GitHub: Contains useful scripts written in Essence#--including example scripts

The Essence# [configuration profile repository](https://github.com/EssenceSharp/Config) on GitHub: Contains the default configuration profiles

The Essence# [tools repository](https://github.com/EssenceSharp/Tools) on GitHub: Contains useful tools for coding in Essence#

# Installation

## Install Using The [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) Installation Program

The [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) Installation Program is what you get when you use the [DOWNLOADS](https://essencesharp.codeplex.com/releases/view/126089) tab or the [HOME](https://essencesharp.codeplex.com/) page to download Essence#. It's a Windows executable file named {{Essence#_<releaseName>_Setup.exe}}, where <releaseName> is the name of the release. To use it to install Essence#, simply execute it, select which components you wish to install (by default, all components are selected for installation, but you can deselect any you don't want or need,) choose the desired installation folders (there's one location for the Essence# code base and another for the Visual Studio project which contains the C# code that implements the Essence# compiler and run time system.)

When selecting the destination folder for the installation of the Essence# code base--especially when installing a new version subsequent to your initial installation--you should keep in mind the guidelines elucidated in the section on [integration issues](integration-issues). In fact, when installing subsequent versions, you should consider using Git instead of using the [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) installer. The next three sections explain what you need to know to do that.

The [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) installer also sets the value of the _%EssenceSharpPath%_ to be whatever installation folder you select when installing the Essence# code base. The construct _%EssenceSharpPath%_ is a [Windows environment variable](http://ss64.com/nt/syntax-variables.html) reference. The Essence# run time system uses the value of the _%EssenceSharpPath%_  [environment variable](http://ss64.com/nt/syntax-variables.html) to locate the active set of [configuration profiles](configuration-profiles), the Essence# [Standard Library](Standard-Library), and Essence# scripts. 

If the _%EssenceSharpPath%_ [environment variable](http://ss64.com/nt/syntax-variables.html) is not defined, then the Essence# run time uses the path _%UserProfile%\My Documents\Developer\EssenceSharp_ as the fallback location. That path is also the default used by the [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) installer to install Essence#, if you use the default installation location and don't change it. 

The [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) installer does not modify the system or user %PATH% [environment variables](http://ss64.com/nt/syntax-variables.html), because of the risk of damaging your system (there's a hard limit to the size of the string value for an [environment variable](http://ss64.com/nt/syntax-variables.html), and automatically adding more search paths may cause the size limit to be exceeded.) 

For more information on [Windows environment variables](http://ss64.com/nt/syntax-variables.html), click on one of the foregoing "environment variable" links . For information on [setting Windows environment variables](setting Windows environment variables) using the Windows Control Panel, [click here](http://www.computerhope.com/issues/ch000549.htm).

## Installation And Configuration Of _EssenceSharp.dll_ And The ES Script Runner

This section explains how to install the Essence# binaries. 

The Essence# binary executables may be located wherever you prefer to put them, although the _default_ location is _%EssenceSharpPath%\Bin._ As explained above in the section on using the [NSIS](http://en.wikipedia.org/wiki/Nullsoft_Scriptable_Install_System) installer, the construct _%EssenceSharpPath%_ is a [Windows environment variable](http://ss64.com/nt/syntax-variables.html) reference. The Essence# run time system uses the value of the _%EssenceSharpPath%_  [environment variable](http://ss64.com/nt/syntax-variables.html) to locate the active set of [configuration profiles](configuration-profiles), the Essence# [Standard Library](Standard-Library), and Essence# scripts. 

If the _%EssenceSharpPath%_ [environment variable](http://ss64.com/nt/syntax-variables.html) is not defined, then the Essence# run time uses the path _%UserProfile%\My Documents\Developer\EssenceSharp_ as the fallback location. 

For more information on [Windows environment variables](http://ss64.com/nt/syntax-variables.html), click on one of the foregoing "environment variable" links . For information on [setting Windows environment variables](setting Windows environment variables) using the Windows Control Panel, [click here](http://www.computerhope.com/issues/ch000549.htm).

The executable for the ES tool that runs Essence# scripts must be in the same folder as _EssenceSharp.dll,_ and both must be in the same folder as the .DLL files that implement Microsoft's DLR. That's because none of the required .DLL files are installed in the [Global Assembly Cache](http://en.wikipedia.org/wiki/Global_Assembly_Cache). Installing .DLL files in the [Global Assembly Cache](http://en.wikipedia.org/wiki/Global_Assembly_Cache) requires Administrator privilege, so doing that is left to you, if you so wish.

The binary executables as distributed reside in sub-folders of the directory _%EssenceSharpPath%\Bin._ The folders directly contained by _%EssenceSharpPath%\Bin_ represent the processor architecture (x86, x64). The next level of folders represent the version of the .Net framework: 35 -> .Net 3.5; 40 -> .Net 4.0+.

That means that there are 4 versions of **es.exe:** One for 32-bit .Net 3.5, one for 32-bit .Net 4.0, one for 64-bit .Net 3.5 and one for 64-bit .Net 4.0.  The same is true of the .DLL files used by **es.exe.** If you need a version of **es.exe** (and the .DLLs it requires) for versions of the .Net framework later than version 4.0, you will have to use Visual Studio to compile them. The C# code that implements Essence# has been compiled and tested against .Net 4.5 and .Net 4.5.1; no changes to the configuration parameters are required for versions of the .Net framework later than 4.0, other than selecting a different version of the .Net framework on the Visual Studio _Properties_ page for both the ES project and the EssenceSharp project. Compiling for .Net 3.5 requires additional changes to the configuration parameters, and requires a different version of the assemblies for the DLR.

**Unless _es.exe_ is in one of the search paths specified by your %PATH% environment variable, you won't be able to run it from the command line unless you make the folder that contains _es.exe_ the current directory.** So you will probably want to modify your %PATH% environment variable so that the **es.exe** program resides in one of the search paths defined by the value of the %PATH% environment variable. Alternatively, you may copy **es.exe** and the .DLLs on which it depends from the _%EssenceSharpPath%\Bin_ folder to some folder which is already one of the specified search paths. 

The  _%EssenceSharpPath%\Bin_ folder as distributed includes a Git repository, which can be connected to the [master branch on GitHub](https://github.com/EssenceSharp/Bin). Once that connection is made, updates can be downloaded using Git. That will also give you access to updates before they are published on CodePlex (although not always.)

## Ad-Hoc ("A La Carte") Installation 

The **REQUIRED** and/or default locations for installing the individual components of Essence# depend upon the value or the _%EssenceSharpPath%_ [environment variable](http://ss64.com/nt/syntax-variables.html). As explained above, the construct _%EssenceSharpPath%_ is a [Windows environment variable](http://ss64.com/nt/syntax-variables.html) reference. The Essence# run time system uses the value of the _%EssenceSharpPath%_  [environment variable](http://ss64.com/nt/syntax-variables.html) to locate the active set of [configuration profiles](configuration-profiles), the Essence# [Standard Library](Standard-Library), and Essence# scripts. 

If the _%EssenceSharpPath%_ [environment variable](http://ss64.com/nt/syntax-variables.html) is not defined, then the Essence# run time uses the path _%UserProfile%\My Documents\Developer\EssenceSharp_ as the fallback location. 

For more information on [Windows environment variables](http://ss64.com/nt/syntax-variables.html), click on one of the foregoing "environment variable" links . For information on [setting Windows environment variables](setting Windows environment variables) using the Windows Control Panel, [click here](http://www.computerhope.com/issues/ch000549.htm).

### The Essence# [Configuration Profiles](Configuration-Profiles)

The **REQUIRED** location of the Essence# [configuration profiles](configuration-profiles)(configuration-profiles) folder is _%EssenceSharpPath%\Config._ The name of the folder that contains the [configuration profiles](configuration-profiles)(configuration-profiles) must be _Config,_ and that folder must reside at _%EssenceSharpPath%._ 

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# [configuration profiles](configuration-profiles)(configuration-profiles)(configuration-profiles) folder includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Config).) Once you have the Essence# [configuration profiles](configuration-profiles)(configuration-profiles)(configuration-profiles) folder with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# [configuration profiles](configuration-profiles)(configuration-profiles)(configuration-profiles) folder using Git.

### The Essence# [Standard Library](Standard-Library)

The default location of the Essence# [Standard Library](Standard-Library)(Standard-Library) is _%EssenceSharpPath%\Source\Libraries\Standard.lib._ The name of the folder that contains it must be _Standard.lib,_ and by default that folder resides at _%EssenceSharpPath%\Source\Libraries._ But the [Standard Library](Standard-Library)(Standard-Library) (i.e, the _Standard.lib_ folder) may reside at any location specified as one of the library search paths by the set of of active [configuration profiles](configuration-profiles).

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# [Standard Library](Standard-Library)(Standard-Library)(Standard-Library) includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Standard.lib).) Once you have the Essence# [Standard Library](Standard-Library)(Standard-Library)(Standard-Library) with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# [Standard Library](Standard-Library)(Standard-Library)(Standard-Library) using Git.

### The Essence# Extensions Library

The default location of the Essence# Extensions Library is _%EssenceSharpPath%\Source\Libraries\Extensions.lib._ The name of the folder that contains it must be _Extensions.lib,_ and by default that folder resides at _%EssenceSharpPath%\Source\Libraries._ But the Extensions Library (i.e, the _Extensions.lib_ folder) may reside at any location specified as one of the library search paths by the set of of active [configuration profiles](configuration-profiles).

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# Extensions Library includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Extensions.lib).) Once you have the Essence# Extensions Library with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# Extensions Library using Git.

### The Essence# Contributions Library

The default location of the Essence# Contributions Library is _%EssenceSharpPath%\Source\Libraries\Contributions.lib._ The name of the folder that contains it must be _Contributions.lib,_ and by default that folder resides at _%EssenceSharpPath%\Source\Libraries._ But the Contributions Library (i.e, the Contributions.lib_ folder) may reside at any location specified as one of the library search paths by the set of of active [configuration profiles](configuration-profiles).

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# Contributions Library includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Contributions.lib).) Once you have the Essence# Contributions Library with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# Contributions Library using Git.

### Essence# Shared Scripts

The **REQUIRED** location of the Essence# shared scripts folder is _%EssenceSharpPath%\Source\Scripts._ The name of the folder that contains the shared scripts must be _Scripts,_ and that folder must reside at _%EssenceSharpPath%\Source._ But scripts may reside in any folder specified as one of the script search paths by the set of of active [configuration profiles](configuration-profiles).

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# shared scripts folder includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Scripts).) Once you have the Essence# shared scripts folder with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# shared scripts folder using Git.

### Essence# Tools

The default location of the Essence# tools folder is _%EssenceSharpPath%\Tools._ By default, the name of the folder that contains the tools is _Tools,_ and by default that folder resides at _%EssenceSharpPath%._ But there are no system constraints or dependencies on the location of the _Tools_ folder.

As installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer, the  Essence# tools folder includes a Git repository.  If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone in Desktop** button on the [GitHub page for the repository](https://github.com/EssenceSharp/Tools).) Once you have the Essence# tools folder with an associated Git repository that is linked to the master branch hosted on GitHub, you can and should get updates to the Essence# tools folder using Git.

### The Essence# Visual Studio Project Folder
Unless you want to browse the C# source code that implements Essence#, want to debug the code, want to make contributions to the compiler or the run time system, want to fork the project, or want to compile the compiler and run time system for later versions of .Net, you have no need to download or install the Essence# Visual Studio project folder.

The Visual Studio project folder installed by the [NSIS](http://en.wikipedia.org/wiki/NSIS) installer includes a Git repository. The version you will get if you use the **Download** link on the **SOURCE CODE** tab does **not** include a Git repository. If you have a version that doesn't have a Git repository, you have to clone the repository (using the **Clone** button on the **SOURCE CODE** page) to get one. You will also need a working familiarity with using Git.  Once you have a version of the Essence# Visual Studio project folder and a Git repository that is linked to the master branch hosted on CodePlex, you can and should get updates to the Visual Studio project using Git.


_
The essence of OOP: It's all messages, all the time._

BeginPackage["SafeFileSystemTraversal`"]
(* Summary:
This package contains mathematica functions written for Dr. Bartels' lab group to ensure safe and OS agnostic directory traversal, directory creation, and filedate extraction.
*)


DOSPathLimit::usage="Holds the character limit for all DOS paths pre-Win10";

DOSFileNameLimit::usage="Holds the character limit for all DOS filenames pre-Win10";

UNIXPathLimit::usage = "Finds the path limit for MOST UNIX-like OSes that use GNU userspace tools.";

UNIXFileNameLimit::usage = "Finds the filename limit for MOST UNIX-like OSes that use GNU userspace tools.";

RootDir::usage="RootDir holds the OS specific version of the system's root directory. In windows this is 'C:\\'.";

AbsolutePathLimit::usage="AbsolutePathLimit holds the integer maximum string length for paths based on the operating system.";
AbsoluteFileNameLimit::usage = "AbsoluteFileNameLimit holds the integer maximum string length for files and folders based on the operating system."

SetPathLimits::usage = "Sets the root directory and file path limits based on what the Operating System is. WARNING:  This is only tested to work with MMA 10-11.  It also assumes all other operating systems are UNIX-like.";

GetFolderName::usage="GetFolderName['PathStr'] will extract the folder name from the full path. THIS HAS ONLY been tested with ASCII paths.";

SafelyCreateDirectory::usage = "SafelyCreateDirectory['PathStr'] is a safer version of CreateDirectory[PathStr]. It will do nothing if the directory already exists. It will create a default if the desired path cannot be written to.";

YamaguchiWorst2014Pattern::usage = "Holds the string pattern for dates in form of 'April 20th, 1992.'";

YamaguchiWorse2014PatternPt2::usage = "Holds times in the 24 hour format of 'H04M20";

Yamaguchi2014Pattern::usage = "Holds the 24 hour date and time in the format of 'Y1992Z04D20H04M20'";
Yamaguchi2015Pattern::usage = "Holds the 24 hour date and time in the format of '19920420.0420'";
Yamaguchi2016ModernPattern::usage = "Holds the 24 hour date and time in the format of '19920420T0420'";

CurrentCentury::usage = "CurrentCentury stores the conversion factor between the full four year format and the two year format. WARNING! This check will fail if any n+1 samples are from different centuries.";

ReadSampleDatePre2014::usage = 
"ReadSampleDateDelta['filename'] will return a
DateObject IFF the filename string contains a parsable date in the format Y00Z00D00H00M00 corresposing to (Y)YY(Z)MM(D)DD(H)HH(M)mm";

ReadSampleDate2015::usage = "ReadSampleDate2015['filename'] will return a DateObject IFF filename has a four year format OR a two year format;
	If four year, the date format contained will be 'YYYYMMDD.HHmmss', of length 15;
	If two year, the date format contained will be 'YYMMDD.HHmmss', of length 13.";

ReadSampleDatePost2016::usage = "ReadSampleDatePost2016['filename'] will return a DateObject IFF filename has a four year format OR a two year format;
	If four year, the date format contained will be 'YYYYMMDDTHHmmss', of length 15;
	If two year, the date format contained will be 'YYMMDDTHHmmss', of length 13.";

ReadSampleFromParentDir::usage = "ReadSampleFromParentDir['filename'] is the worst, most resource intensive, and final resort to get date information from a sample. It will check all parent directories for a parsable date, and output that date with any timestamps from the samples added. This should 
output a DateObject if it worked, but should output Indeterminate if it fails.";

FailedSampleDate::usage = "FailedSampleDate['filename'] throws a $Failed exception, as a date could not be parsed by any known naming patterns. We will NOT be using the files date modified data, as this easily changes just with a copy or moving of files.";

OneTrueReadSampleDate::usage = "OneTrueReadSampleDate['filename'] combines all known dating patterns for Mr. Yamaguchi into the one ReadSampleDate program to rule them all, the OneTrueReadSampleDate! If this fails to find a date, an error shall be thrown and you shall (not) pass.";

ImageContainingFolderName::usage = "Holds the image containing directory's name.";

WorkingDirectory::usage = "Holds the pull path to the image contaning directory.";

files::usage = "files holds the 1D, but unsorted, array of image file path names.";

sortfiles::usage = "sortfiles[[i,j]] holds the sorted matrix of image file paths, usually sorted by wavelength.";

sortfilesfunc::usage="sortfilesfunc[] reads in and sorts the 'files' array into an iXj matrix based on 'i' sets of wavelengths with 'j' readings.";

pCordMat::usage = "pCordMat stores a copy of sortfiles so that a future function may assign to pCordMat each image's unique point of interest's coordinates";

SetDirandImport::usage = "SetDirandImport['workdir'] attempts to set the current directory to 'workdir' and import all images and saved state matricies that might exist in 'workdir.' This will quietly fail if the directory or any expected files are missing.";

IspCordLoaded::usage = "IspCordLoaded is a boolean flag that controls if drift offests from the mypCord have already been imported. This is FALSE by default."

AutoSafeLoadmypCord::usage = "AutoSafeLoadmypCord will load my_pCord.mat into pCordMat IFF the file exists in the working directory AND the imported matrix has the correct dimensions.";

overWritepCordmat::usage = "overWritepCordmat is a button system to prompt the user from within a notebook environment if they want to load or save pCordMat.";

ExperimentalConditions::usage = "ExperimentalConditions stores the user's input experimental conditions." ;

SetExperimentalConditions::usage = "Prompts the user to describe experimental conditions, and saves this as an OS friendly string.";

ExperimentName::usage = "ExperimentalName stores the user's input experiment name.";

SetExperimentName::usage = "Prompts the user to input the experiment name, and saves this as an OS friendly string.";

Begin["`Private`"]

DOSPathLimit:= 255;

DOSFileNameLimit:= 255;

UNIXPathLimit:= ToExpression[RunProcess[{"getconf", "PATH_MAX", "/dev/sda1"},"StandardOutput"]];

UNIXFileNameLimit:= ToExpression[RunProcess[{"getconf", "NAME_MAX", "/dev/sda1"},"StandardOutput"]];

RootDir=.;

AbsolutePathLimit=.;

AbsoluteFileNameLimit=.;

SetPathLimits=
If[$OperatingSystem=="Windows",
	SafeFileSystemTraversal`RootDir="C:\\";
	SafeFileSystemTraversal`AbsolutePathLimit=SafeFileSystemTraversal`DOSPathLimit;
	SafeFileSystemTraversal`AbsoluteFileNameLimit=SafeFileSystemTraversal`DOSFileNameLimit,
(*Else:*)
	SafeFileSystemTraversal`RootDir="~/";
	SafeFileSystemTraversal`AbsolutePathLimit=SafeFileSystemTraversal`UNIXPathLimit;
	SafeFileSystemTraversal`AbsoluteFileNameLimit=SafeFileSystemTraversal`UNIXFileNameLimit
	];

GetFolderName[PathStr_]:=
Last[StringSplit[PathStr,$PathnameSeparator]];


SafelyCreateDirectory[PathStr_]:=
Module[
	{
	(*The null hypothesis is that it is NOT safe to create the Directory*)
	SafeToWrite = False,
	Dir0 = PathStr,
	Folder0=SafeFileSystemTraversal`GetFolderName[PathStr]
	},
	(*Verify proposed directory is less than OS path limits and that it does not exist.*)
	If[(StringLength[Dir0]<=SafeFileSystemTraversal`AbsolutePathLimit && StringLength[Folder0]<=SafeFileSystemTraversal`AbsoluteFileNameLimit
&&!DirectoryQ[Dir0]),
		SafeToWrite=True
		];
	(*If the proposed foldername is too long, trim it to the OS's Limit*)
	If[StringLength[Folder0] >
		SafeFileSystemTraversal`AbsoluteFileNameLimit,
     		Folder0 = StringDrop[
			Folder0, (ToExpression[
			SafeFileSystemTraversal`AbsoluteFileNameLimit] - StringLength[Folder0]-StringLength[$HomeDirectory])
			]
	];

	(*If the home directory ALSO cannot be written to, Arceus help you, as this whole function will break!*)
	If[!SafeToWrite,
		Dir0=FileNameJoin[{$HomeDirectory, Folder0}]
	];

	If[!DirectoryQ[Dir0],Quiet[CreateDirectory[Dir0]]];

	(*Replace the input with the functions output*)
	PathStr=Dir0
		];
	SetAttributes[SafeFileSystemTraversal`SafelyCreateDirectory,HoldFirst];


YamaguchiWorst2014Pattern = __ ~~ RegularExpression["\\d\\d\\d\\d"];

YamaguchiWorse2014PatternPt2 = "H" ~~ __ ~~ "M" ~~ DigitCharacter ..;

Yamaguchi2014Pattern = 
  "Y" ~~ __ ~~ "Z" ~~ __ ~~ "D" ~~ __ ~~ "H" ~~ __ ~~ "M" ~~ 
   DigitCharacter ..;
   
Yamaguchi2015Pattern = DigitCharacter .. ~~ "." ~~ DigitCharacter ..;

Yamaguchi2016ModernPattern = 
  DigitCharacter .. ~~ "T" ~~ DigitCharacter ..;


CurrentCentury = 
  DateList[{ToExpression[DateString[{"Year"}]] - 
     ToExpression[DateString[{"YearShort"}]], 1, 1, 0, 0, 0.}];


ReadSampleDatePre2014[filename_] :=
  Module[
   {
    dateholder = StringCases[ToString[filename],SafeFileSystemTraversal`Yamaguchi2014Pattern]
    },
   
   dateholder = StringCases[First[dateholder], DigitCharacter ..];
   
   dateholder = DateList[ToExpression[dateholder]];
   (*We must correct Mathematicas assumption that short years are \
only happening during the first century CE, 
   but be careful not to over correct!*)
   dateholder = 
    DateList[(dateholder + SafeFileSystemTraversal`CurrentCentury) - {0, 1, 1, 0, 0, 0.}];
   (*Return:*)
   DateObject[dateholder, TimeZone -> $TimeZone]
   ];


ReadSampleDate2015[filename_] :=
  Module[
   {
    dateholder = StringCases[ToString[filename], SafeFileSystemTraversal`Yamaguchi2015Pattern],
    dateoutput = DateList[]
    },
   
   datelength = StringLength[First[dateholder]];
   Which[
    datelength == 15,
    dateoutput = 
     DateList[{First[dateholder], {"Year", "Month", "Day", "T", 
        "Hour24", "Minute", "Second"}}
      		],
    	
    datelength == 13,
    dateoutput = 
     DateList[{First[dateholder], {"YearShort", "Month", "Day", "T", 
        "Hour24", "Minute", "Second"}}
      		],
    (*Else*)
    datelength != 15 && datelength != 13,
    dateoutput = Indeterminate
    	];
   	Clear[datelength];
   (*return:*)
   DateObject[dateoutput, TimeZone -> $TimeZone]
   		
   		];


ReadSampleDatePost2016[filename_] :=
  Module[
   {
    dateholder = 
     StringCases[ToString[filename], SafeFileSystemTraversal`Yamaguchi2016ModernPattern],
    dateoutput = DateList[]
    },
   
   datelength = StringLength[First[dateholder]];
   Which[
    datelength == 15,
    dateoutput = 
     DateList[{First[dateholder], {"Year", "Month", "Day", "T", 
        "Hour24", "Minute", "Second"}}
      		],
    	
    datelength == 13,
    dateoutput = 
     DateList[{First[dateholder], {"YearShort", "Month", "Day", "T", 
        "Hour24", "Minute", "Second"}}
      		],
    (*Else*)
    datelength != 15 && datelength != 13,
    dateoutput = Indeterminate
    	];
   	Clear[datelength];
   (*return:*)
   DateObject[dateoutput, TimeZone -> $TimeZone]
   		
   		];


ReadSampleFromParentDir[filename_] :=
 Module[
  {
   pathlist = Reverse[FileNameSplit[filename]],
   dsholder = {},
   (*The null hypothesis is that we cannot find a date! The boolean flag 'datefound' will control IF we can even attempt to extract a
	date, based on known dating paradigms.*)
   timeholder = Indeterminate,
   dateholder = Indeterminate,
   (*We are here because the file has no parsable date, 
   so we must start at n = 2.*)
   n = 2
   },
  
  (*
  Traverse through directory tree until either we hit RootDir or we find a date.
  *)
  
  While[ !DateObjectQ[dateholder] && n < Length[pathlist],
   
   dsholder = StringCases[pathlist[[n]], SafeFileSystemTraversal`YamaguchiWorst2014Pattern, 1];
   
   If[StringContainsQ[pathlist[[n]], SafeFileSystemTraversal`YamaguchiWorst2014Pattern],
    dateholder = DateObject[DateString[dsholder]]
    	];
   If[!DateObjectQ[dateholder], dateholder = Indeterminate ; n++];
   
   ];
  (*Next, if we did find a date, 
  see if we can find a timestamp  to append to dateholder from the OG file;*)
  If[
   DateObjectQ[dateholder] && 
    StringContainsQ[pathlist[[1]], SafeFileSystemTraversal`YamaguchiWorse2014PatternPt2],
   dsholder = 
    StringCases[pathlist[[1]], SafeFileSystemTraversal`YamaguchiWorse2014PatternPt2, 1];
   dsholder = 
    ToExpression[Flatten[StringCases[dsholder, DigitCharacter ..]]];
   	(*Add zero seconds to the timestamp, 
   if only to keep bytesize consistent with all other dateObjects.*)
		If[Length[dsholder] == 2,
			dsholder = AppendTo[dsholder, 0.]
			];
   timeholder = TimeObject[dsholder];
   ,
  (*Else:*)
   timeholder = Indeterminate;
   
   ];
  
  (*return:*)
  	
  If[DateObjectQ[dateholder] && TimeObjectQ[timeholder],
   DateObject[dateholder, timeholder],
   (*Else:*)
   dateholder
   	]
  
  ];


FailedSampleDate[filename_] :=
  Module[
   {},
   Panel[StringJoin["A parsable date in file path\n", filename, 
   "\n could not be found. A $Failed error will now be thrown!"]];
   (*return:*)
   Throw[$Failed, "This date import failed!"]
   ];


OneTrueReadSampleDate[filename_] :=
 Module[
  		{
   		(*A failed read should should fail to output a DateObject, but to be safe, a correct default output will be generated.*)
   		dateoutput = DateObject[DateList[], TimeZone -> $TimeZone],
   		(*The null hypothesis is that we cannot find a date! The boolean flag 'datefound' will control IF we can even attempt to extract a date, based on known dating paradigms.*)
   		datefound = False
   		},
  If[
   StringContainsQ[filename, SafeFileSystemTraversal`Yamaguchi2014Pattern],
   dateoutput = Quiet[SafeFileSystemTraversal`ReadSampleDatePre2014[filename]];
   If[DateObjectQ[dateoutput], datefound = True]
   	];
  If[
   !datefound && StringContainsQ[filename, SafeFileSystemTraversal`Yamaguchi2015Pattern],
   dateoutput = Quiet[SafeFileSystemTraversal`ReadSampleDate2015[filename]];
   If[DateObjectQ[dateoutput], datefound = True]
   	];
  If[
   !datefound && 
    StringContainsQ[filename, SafeFileSystemTraversal`Yamaguchi2016ModernPattern],
   dateoutput = Quiet[SafeFileSystemTraversal`ReadSampleDatePost2016[filename]];
   If[DateObjectQ[dateoutput], datefound = True]
   	];
  (*The final attempt, look for dates in parent directories.*)
  If[
   !datefound,
   dateoutput = Quiet[SafeFileSystemTraversal`ReadSampleFromParentDir[filename]];
   If[DateObjectQ[dateoutput], datefound = True]
   	];
  (*return:*)
  If[datefound, dateoutput,
   (*Else If:*)
   Catch[FailedSampleDate[filename], "This date import failed!"],
   (*Else*)
   $Failed
   ]
  
  ];


ImageContainingFolderName=Indeterminate;


WorkingDirectory=Indeterminate;


files=Indeterminate;


sortfiles=Indeterminate;


sortfilesfunc[]:=
Table[
files[[Flatten[Position[StringContainsQ[files,StringJoin["F",ToString[i]]],True]]]],{i, 1, 7, 1}
	];

pCordMat=Indeterminate;

SetDirandImport[workdir_]:=
Module[
		{
		direxists = False,
		dirset = False
		},
		If[StringQ[workdir] && DirectoryQ[workdir], direxists = True];
		
		If[direxists,
			SafeFileSystemTraversal`WorkingDirectory = workdir;
			SetDirectory[SafeFileSystemTraversal`WorkingDirectory];
			dirset = True
			];
		If[dirset,
			SafeFileSystemTraversal`files=FileNames["*.tif"];
			SafeFileSystemTraversal`sortfiles=SafeFileSystemTraversal`sortfilesfunc[];
			SafeFileSystemTraversal`pCordMat=SafeFileSystemTraversal`sortfiles,
		(*Else If*)
			SafeFileSystemTraversal`files={};
			SafeFileSystemTraversal`sortfiles={};
			SafeFileSystemTraversal`pCordMat={},
		(*Else*)
			SafeFileSystemTraversal`files=$Failed;
			SafeFileSystemTraversal`sortfiles=$Failed;
			SafeFileSystemTraversal`pCordMat=$Failed
		];
];

IspCordLoaded = False;

AutoSafeLoadmypCord[]:=
Module[

		{
		tmpmatp,
		FileMatDim = Dimensions[SafeFileSystemTraversal`sortfiles]
		},

		If[FileExistsQ["my_pCord.mat"],tmpmatp=<<"my_pCord.mat"];
		
	(*Action: Ensure sortfiles is a non-empty matrix and check dimensions. Load pCordMat if they match and are non-empty*)
		If[(FileMatDim!={}&&FileMatDim!={0}),
			If[
			Dimensions[tmpmatp]=={FileMatDim[[1]],FileMatDim[[2]],2} &&
			ArrayQ[tmpmatp, _ , NumericQ],
			SafeFileSystemTraversal`pCordMat=tmpmatp;
			IspCordLoaded=True;
				],
			(*Else:*)
			$Failed,
			$Failed
			];
		Clear[tmpmatp,FileMatDim];
	];

overWritepCordmat=
Panel[
		ButtonBar[
		{
		"Save":>(SafeFileSystemTraversal`pCordMat>>"my_pCord.mat"),"Load":>If[FileExistsQ["my_pCord.mat"],SafeFileSystemTraversal`AutoSafeLoadmypCord,Print["File: my_pCord.mat does not yet exist!"]]
		}
				  ],
		"Do you wish to overwrite/save the current drift offsets,\n or load the previous drift offsets?"];

ExperimentalConditions = Indeterminate;

SetExperimentalConditions :=  ToString[Input["Please describe the experiment's conditions."]];

ExperimentName = Indeterminate;

SetExperimentName := ToString[Input["Please input the experiment's name."]];

End[]

EndPackage[]

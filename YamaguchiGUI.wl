BeginPackage["YamaguchiGUI`",{"YamaguchiProcessImg`","SafeFileSystemTraversal`"}]

sfsFlatImg::usage =
"sfsFlatImg['dirMat', 'i','j'] imports a file from a given 2D directory matrix at dirMat[[i,j]], and imports only the first element if the image is multilayered";

setPOIManipFixed::usage="This manipulate field allows a user to correct and set myPCord .";
viewChangePOIMat::usage= "viewChangePOIMat allows a user to .";
s::usage="This stores a public variable for some slier.";

SlideName::usage="This holds the namefield for a slider that contains a POI manipulation.";

dynamicManipPOISlider::usage= "dynamicManipPOISlider[] allows a user to see the state of myPCord and slide right to change any drift offset if needed.";

bottomleftref::usage="Stores the bottom left corner of the selected island box.";
toprightref::usage="Stores the top right corner of the selected island box.";
bottomleftmean::usage="Stores the bottom left corner of the selected box of background.";
toprightmean::usage="Stores the top right corner of the selected box of background.";

imgrefpos::usage= "Stores the reference position of the point of interests";
imgoffsets::usage= "Stores the offsets of the point of interests";
LoadpposData::usage= "LoadpposData[] initializes imgrefpof and imgoffsets, and attempts to load if one exists, or save pposData if said file does not exist."

selectCorners::usage="selectCorners[] allows the user to select a box for POI islands and a box for a sample of background substrate for future correction.";

Begin["`Private`"]

sfsFlatImg[dirMat_,i_,j_]:=
Module[
		{
		tmpimg=Import[dirMat[[i,j]],"ColorSpace"->"Grayscale"]
		},
		(*For whatever reason, Mathematica 10-11 treats single images as length '0', even though in all other scenarios it begins counting at 1. Therefore, if the returned length is greater than zero, we have an animated/layerd image.*)
		(*
		WARNING!: Future changes to default Length[] behavior may break this function!
		*)			
		If[Length[tmpimg]>0,
				tmpimg=Part[tmpimg,1]
		];
		(*Return:*)
		tmpimg
	   ];

setPOIManipFixed[matPass_] := Manipulate[
   If[Quiet[ArrayQ[matPass[[i, ii]], _, NumericQ]],
    possiblepoint = matPass[[i, ii]],
    (*Else*)
    possiblepoint = {100, 100},
    possiblepoint = {100, 100}
    ];
   matPass[[i, ii]] = p; (*As the pointer moves to a point, 
   it overrides the filename's location in matrix b.*)

   currimg = YamaguchiProcessImg`FlatImg[SafeFileSystemTraversal`sortfiles, i, ii];
   showbounds = {Quantile[Flatten[ImageData[currimg]], .05], 
     Quantile[Flatten[ImageData[currimg]], .95]};
   (*Best gamma for pure visibility on consumer screens:
   http://www.ti.com/lit/an/spra361/spra361.pdf
   *)
   ImageAdjust[currimg, 2.2, showbounds],

   {{i, 1, YamaguchiProcessImg`TwoDRowCount[SafeFileSystemTraversal`sortfiles], 1}, 
    Table[j, {j, YamaguchiProcessImg`TwoDRowCount[SafeFileSystemTraversal`sortfiles]}], PopupMenu}
   , {{ii, 1, YamaguchiProcessImg`TwoDColumnCount[SafeFileSystemTraversal`sortfiles], 1}, 
    Table[j -> SafeFileSystemTraversal`sortfiles[[i, j]], {j, YamaguchiProcessImg`TwoDColumnCount[SafeFileSystemTraversal`sortfiles]}], 
    PopupMenu},
   {{p, possiblepoint}, Locator,
    Appearance -> Style["\[CircleDot]", Blue, 24]}

   ];
SetAttributes[setPOIManipFixed, HoldFirst];
viewChangePOIMat[Type_] :=
	If[Type == 1,
		If[(SafeFileSystemTraversal`IspCordLoaded),
    "A file was already loaded, slide right if you wish to verify the offset!",
    "No offset matrix was found, please slide right to manually choose a POI and its offsets.", 
    "An I/O error was detected, please check that Directory[] outputs something correct and that a 'my_pCord.mat' exists!"],
	(*Else:*)
    YamaguchiGUI`setPOIManipFixed[SafeFileSystemTraversal`pCordMat], YamaguchiGUI`setPOIManipFixed[SafeFileSystemTraversal`pCordMat]];

s=.;

SlideName=.;

dynamicManipPOISlider[] := Manipulate[

   Refresh[{YamaguchiGUI`viewChangePOIMat[s]}, TrackedSymbols :> {s}],
   {s, 1, 2, 1, Appearance -> SlideName},

   {SlideName, {"\t", "\t", "\t", "\t", "Manual_Offset"}, 
    Appearance -> "Palette"}];

bottomleftref = {100, 100}; 
toprightref = {200, 200};
bottomleftmean = {300, 300}; 
toprightmean = {350, 350};

imgrefpos = Indeterminate;
imgoffsets = Indeterminate;
LoadpposData[]:= Module[
			{},
			imgrefpos = SafeFileSystemTraversal`pCordMat[[1, 1]];
			imgoffsets = 
  Partition[Map[# - SafeFileSystemTraversal`pCordMat[[1, 1]] &, Flatten[SafeFileSystemTraversal`pCordMat, 1]], 
   YamaguchiProcessImg`TwoDColumnCount[SafeFileSystemTraversal`sortfiles]];
			(*Check if pposdata.mat exists, load values from it if a previous \
session saved them, otherwise write a new one with default values.*)
		If[! FileExistsQ["pposdata.mat"],
 Put[YamaguchiGUI`imgrefpos, YamaguchiGUI`imgoffsets, YamaguchiGUI`bottomleftref, YamaguchiGUI`toprightref,
  YamaguchiGUI`bottomleftmean, YamaguchiGUI`toprightmean, "pposdata.mat"],
 str = OpenRead["pposdata.mat"]; {YamaguchiGUI`imgrefpos, imgoffsets,
   YamaguchiGUI`bottomleftref, YamaguchiGUI`toprightref, YamaguchiGUI`bottomleftmean,
   YamaguchiGUI`toprightmean} = {Read[str], Read[str], Read[str], Read[str],
   Read[str], Read[str]}; Close[str]]

];

selectCorners[] := Manipulate[
(*Move cleaned code here.*)

];

End[]

EndPackage[]

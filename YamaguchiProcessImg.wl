BeginPackage["YamaguchiProcessImg`"]
(* Summary:
This package contains mathematica functions written for the purpose of analyzing transition metal dichalcogenides (TMDs) as moving islands of interests as they undergo reactions.
*)

TwoDRowCount::usage = "TwoDRowCount['list'] gives the integer row count for a 2D data set.";

TwoDColumnCount::usage = "TwoDColumnCount['list']
gives the integer column count for a 2D data set.";

FlatImg::usage = 
"FlatImg['dirMat', 'i','j'] imports a file from a given 2D directory matrix at dirMat[[i,j]], and imports only the first element if the image is multilayered";

IsoDate::usage = 
"IsoDate[] returns the ISO Date with 24 time as a string that is safe for almost all operating systems as file and directory paths.";

guiDimOut::usage = 
"guiDimOut[inmat] outputs a panel with the dimensions and name of said matrix.";

inimg::usage = "inimg['i','j','inputdirectory'] imports an image IFF it was output into inputdirectory in the format of 'i_j.tif' AS A SINGLE LAYER IMAGE!";

inimgflat::usage = "inimgflat['i','j','inputdirectory']:= imports an image IFF it was output into inputdirectory in the format of 'i_j.tif' and FLATTENS the image if multilayered. ONLY use this if somehow, we lost track if a proccessed outputs were saved as single or multilayered.";

truefindbox::usage = "truefindbox['img','crop'] finds the inner 'crop's coordinates within the parent 'img' and returns coordinates useful with Mathematica's ImageTrim[] function.";

FloatPointMode::usage = "FloatPointMode[passlist, sigfig] will return the mode (most common value) of a 1D floatpoint list by IGNORING everything past its given number of significant figures";

GrayScaleByteSize::usage = "Holds Mathematica 10-11's idiotic scaling factor of true U16 values to zero through one, where 'one' is scalled to the GrayScaleByteSize";

SixteenBitMode::usage = "SixteenBitMode['passlist'] returns the true U16 integer mode (most common value) of the list.";

Begin["`Private`"]

TwoDRowCount[list_]:=Length[list[[All]]];

TwoDColumnCount[list_]:=Length[list[[1,All]]];

FlatImg[dirMat_,i_,j_]:=
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

IsoDate[]:=
DateString[{"ISODate","-","Hour24", "-", "Minute"}];

guiDimOut[inmat_] :=
Module[
	{
	dimMat = Dimensions[inmat],
	symMat = SymbolName[Unevaluated[inmat]]
	},
	(*Return:*)
	Panel[dimMat, 
		StringJoin["The '", symMat, "' matrix has dimensions: \n"]
		]
   ];
SetAttributes[YamaguchiProcessImg`guiDimOut, HoldFirst];

inimg[i_,j_,inputdirectory_]:=
Import[
		StringJoin[inputdirectory,$PathnameSeparator,
        ToString[i],"_",ToString[j],".tif"]
	  ];

inimgflat[i_,j_,inputdirectory_]:=
Module[
		{
		tmpimg=Import[StringJoin[inputdirectory,$PathnameSeparator,                                  ToString[i],"_",ToString[j],".tif"]]
		},
			If[Length[tmpimg]>0,
				tmpimg=Part[tmpimg,1]
			  ];
			(*Return:*)
			tmpimg
	];

truefindbox[img_,crop_]:=
Module[
		{
		transmatrix=FindGeometricTransform[img,crop,
		TransformationClass->"Similarity"][[2]]
		},
			tfp={
			{
			transmatrix[[1,1,3]],
			transmatrix[[1,2,3]]
			},
			{
			transmatrix[[1,1,3]]+ImageDimensions[crop][[1]],transmatrix[[1,2,3]]+ImageDimensions[crop][[2]]
			}
				};
			(*Return:*)
			tfp
	];

FloatPointMode[passlist_, sigfig_]:=
Module[
	{
	siglist=SetPrecision[passlist,sigfig]
	},
		(*Return:*)
		Part[Commonest[siglist,1],1]
	];

GrayScaleByteSize=(2^16-1);

SixteenBitMode[passlist_]:=
Module[
	{
		USixteenlist=Round[passlist*GrayScaleByteSize]
	},
		(*Return:*)
		Part[Commonest[USixteenlist,1],1]
	];
End[]

EndPackage[]

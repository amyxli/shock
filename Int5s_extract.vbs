Set Doc = CreateObject ("ADIChart.Document")
Set App = Doc.Application
Set Services = Doc.Services

Sub Int5s_extract ()

	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "proceed"
	RepeatSetAction = kSelectRegion
	RegionSecs = 5
	SelectMode = kSelectBefore
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "fon"
	RepeatSetAction = kSelectRegion
	RegionSecs = 5
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "fon"
	RepeatSetAction = kSelectRegion
	RegionSecs = 10
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "fon"
	RepeatSetAction = kSelectRegion
	RegionSecs = 15
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "fon"
	RepeatSetAction = kSelectRegion
	RegionSecs = 20
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "kis"
	RepeatSetAction = kSelectRegion
	RegionSecs = 5
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "kis"
	RepeatSetAction = kSelectRegion
	RegionSecs = 10
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "kis"
	RepeatSetAction = kSelectRegion
	RegionSecs = 15
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "kis"
	RepeatSetAction = kSelectRegion
	RegionSecs = 20
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "shock"
	RepeatSetAction = kSelectRegion
	RegionSecs = 5
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	
	' Begin MultipleAddToDataPad_Comment
	SourceChannelIndex = kAnyChannelIndex
	Comment = "nothing"
	RepeatSetAction = kSelectRegion
	RegionSecs = 5
	SelectMode = kSelectAfter
	NextComment = ""
	SelectScope = kWholeFile
	Call Doc.MultipleAddToDataPad_Comment (SourceChannelIndex, Comment, RepeatSetAction, RegionSecs, SelectMode, NextComment, SelectScope)
	' End MultipleAddToDataPad_Comment
	

End Sub


Call Int5s_extract ()
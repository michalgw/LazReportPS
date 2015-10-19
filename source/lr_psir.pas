unit lr_psir;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSRuntime;

procedure lr_psirunt(SE: TPSExec; CL: TPSRuntimeClassImporter);

implementation

uses
  LR_Class, Graphics, LR_Intrp, LR_DBComponent, DB, LR_DBSet, Controls, LR_DSet,
  LR_DBRel, Printers, LR_View;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TFPListList_R(Self: TFPList; var T: PPointerList);
begin T := Self.List; end;

(*----------------------------------------------------------------------------*)
procedure TFPListItems_W(Self: TFPList; const T: Pointer; const t1: Integer);
begin Self.Items[t1] := T; end;

(*----------------------------------------------------------------------------*)
procedure TFPListItems_R(Self: TFPList; var T: Pointer; const t1: Integer);
begin T := Self.Items[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TFPListCount_W(Self: TFPList; const T: Integer);
begin Self.Count := T; end;

(*----------------------------------------------------------------------------*)
procedure TFPListCount_R(Self: TFPList; var T: Integer);
begin T := Self.Count; end;

(*----------------------------------------------------------------------------*)
procedure TFPListCapacity_W(Self: TFPList; const T: Integer);
begin Self.Capacity := T; end;

(*----------------------------------------------------------------------------*)
procedure TFPListCapacity_R(Self: TFPList; var T: Integer);
begin T := Self.Capacity; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TFPList(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TFPList) do
  begin
    RegisterMethod(@TFPList.AddList, 'AddList');
    RegisterMethod(@TFPList.Add, 'Add');
    RegisterMethod(@TFPList.Clear, 'Clear');
    RegisterMethod(@TFPList.Delete, 'Delete');
    RegisterMethod(@TFPList.Error, 'Error');
    RegisterMethod(@TFPList.Exchange, 'Exchange');
    RegisterMethod(@TFPList.Expand, 'Expand');
    RegisterMethod(@TFPList.Extract, 'Extract');
    RegisterMethod(@TFPList.First, 'First');
    RegisterMethod(@TFPList.GetEnumerator, 'GetEnumerator');
    RegisterMethod(@TFPList.IndexOf, 'IndexOf');
    RegisterMethod(@TFPList.IndexOfItem, 'IndexOfItem');
    RegisterMethod(@TFPList.Insert, 'Insert');
    RegisterMethod(@TFPList.Last, 'Last');
    RegisterMethod(@TFPList.Move, 'Move');
    RegisterMethod(@TFPList.Assign, 'Assign');
    RegisterMethod(@TFPList.Remove, 'Remove');
    RegisterMethod(@TFPList.Pack, 'Pack');
    RegisterMethod(@TFPList.Sort, 'Sort');
    RegisterMethod(@TFPList.ForEachCall, 'ForEachCall');
    RegisterMethod(@TFPList.ForEachCall, 'ForEachCall');
    RegisterPropertyHelper(@TFPListCapacity_R,@TFPListCapacity_W,'Capacity');
    RegisterPropertyHelper(@TFPListCount_R,@TFPListCount_W,'Count');
    RegisterPropertyHelper(@TFPListItems_R,@TFPListItems_W,'Items');
    RegisterPropertyHelper(@TFPListList_R,nil,'List');
  end;
end;

(*----------------------------------------------------------------------------*)

procedure TFontPixelsPerInch_W(Self: TFont; const T: Integer);
begin Self.PixelsPerInch := T; end;

(*----------------------------------------------------------------------------*)
procedure TFontPixelsPerInch_R(Self: TFont; var T: Integer);
begin T := Self.PixelsPerInch; end;

(*----------------------------------------------------------------------------*)
procedure TFontIsMonoSpace_R(Self: TFont; var T: boolean);
begin T := Self.IsMonoSpace; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TFont(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TFont) do
  begin
    RegisterMethod(@TFont.IsDefault, 'IsDefault');
    RegisterVirtualMethod(@TFont.IsEqual, 'IsEqual');
    RegisterPropertyHelper(@TFontIsMonoSpace_R,nil,'IsMonoSpace');
    RegisterPropertyHelper(@TFontPixelsPerInch_R,@TFontPixelsPerInch_W,'PixelsPerInch');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesCount_R(Self: TfrVariables; var T: Integer);
begin T := Self.Count; end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesName_R(Self: TfrVariables; var T: String; const t1: Integer);
begin T := Self.Name[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesValue_W(Self: TfrVariables; const T: Variant; const t1: Integer);
begin Self.Value[t1] := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesValue_R(Self: TfrVariables; var T: Variant; const t1: Integer);
begin T := Self.Value[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesVariable_W(Self: TfrVariables; const T: Variant; const t1: String);
begin Self.Variable[t1] := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrVariablesVariable_R(Self: TfrVariables; var T: Variant; const t1: String);
begin T := Self.Variable[t1]; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrVariables(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrVariables) do
  begin
    RegisterConstructor(@TfrVariables.Create, 'Create');
    RegisterMethod(@TfrVariables.Clear, 'Clear');
    RegisterMethod(@TfrVariables.Delete, 'Delete');
    RegisterMethod(@TfrVariables.IndexOf, 'IndexOf');
    RegisterPropertyHelper(@TfrVariablesVariable_R,@TfrVariablesVariable_W,'Variable');
    RegisterPropertyHelper(@TfrVariablesValue_R,@TfrVariablesValue_W,'Value');
    RegisterPropertyHelper(@TfrVariablesName_R,nil,'Name');
    RegisterPropertyHelper(@TfrVariablesCount_R,nil,'Count');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnNext_W(Self: TlrUserDSControl; const T: String);
begin Self.PSOnNext := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnNext_R(Self: TlrUserDSControl; var T: String);
begin T := Self.PSOnNext; end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnFirst_W(Self: TlrUserDSControl; const T: String);
begin Self.PSOnFirst := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnFirst_R(Self: TlrUserDSControl; var T: String);
begin T := Self.PSOnFirst; end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnCheckEof_W(Self: TlrUserDSControl; const T: String);
begin Self.PSOnCheckEof := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrUserDSControlPSOnCheckEof_R(Self: TlrUserDSControl; var T: String);
begin T := Self.PSOnCheckEof; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrUserDSControl(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrUserDSControl) do
  begin
    RegisterPropertyHelper(@TlrUserDSControlPSOnCheckEof_R,@TlrUserDSControlPSOnCheckEof_W,'PSOnCheckEof');
    RegisterPropertyHelper(@TlrUserDSControlPSOnFirst_R,@TlrUserDSControlPSOnFirst_W,'PSOnFirst');
    RegisterPropertyHelper(@TlrUserDSControlPSOnNext_R,@TlrUserDSControlPSOnNext_W,'PSOnNext');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDataSource_W(Self: TLRDataSetControl; const T: string);
begin Self.DataSource := T; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDataSource_R(Self: TLRDataSetControl; var T: string);
begin T := Self.DataSource; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFilter_W(Self: TLRDataSetControl; const T: string);
begin Self.Filter := T; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFilter_R(Self: TLRDataSetControl; var T: string);
begin T := Self.Filter; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFieldCount_R(Self: TLRDataSetControl; var T: integer);
begin T := Self.FieldCount; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlRecordCount_R(Self: TLRDataSetControl; var T: integer);
begin T := Self.RecordCount; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlEOF_R(Self: TLRDataSetControl; var T: boolean);
begin T := Self.EOF; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlActive_W(Self: TLRDataSetControl; const T: boolean);
begin Self.Active := T; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlActive_R(Self: TLRDataSetControl; var T: boolean);
begin T := Self.Active; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFieldName_R(Self: TLRDataSetControl; var T: String; const t1: Integer);
begin T := Self.FieldName[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFieldValue_R(Self: TLRDataSetControl; var T: Variant; const t1: String);
begin T := Self.FieldValue[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlFieldText_R(Self: TLRDataSetControl; var T: String; const t1: String);
begin T := Self.FieldText[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDisplayLabel_R(Self: TLRDataSetControl; var T: String; const t1: String);
begin T := Self.DisplayLabel[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDisplayWidth_R(Self: TLRDataSetControl; var T: Integer; const t1: String);
begin T := Self.DisplayWidth[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControllrDataSource_R(Self: TLRDataSetControl; var T: TDataSource);
begin T := Self.lrDataSource; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControllrDBDataSet_R(Self: TLRDataSetControl; var T: TfrDBDataSet);
begin T := Self.lrDBDataSet; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDataSet_W(Self: TLRDataSetControl; const T: TDataSet);
begin Self.DataSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TLRDataSetControlDataSet_R(Self: TLRDataSetControl; var T: TDataSet);
begin T := Self.DataSet; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TLRDataSetControl(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TLRDataSetControl) do
  begin
    RegisterMethod(@TLRDataSetControl.Open, 'Open');
    RegisterMethod(@TLRDataSetControl.Close, 'Close');
    RegisterMethod(@TLRDataSetControl.First, 'First');
    RegisterMethod(@TLRDataSetControl.Next, 'Next');
    RegisterMethod(@TLRDataSetControl.Last, 'Last');
    RegisterMethod(@TLRDataSetControl.Prior, 'Prior');
    RegisterPropertyHelper(@TLRDataSetControlDataSet_R,@TLRDataSetControlDataSet_W,'DataSet');
    RegisterPropertyHelper(@TLRDataSetControllrDBDataSet_R,nil,'lrDBDataSet');
    RegisterPropertyHelper(@TLRDataSetControllrDataSource_R,nil,'lrDataSource');
    RegisterPropertyHelper(@TLRDataSetControlFieldValue_R,nil,'FieldValue');
    RegisterPropertyHelper(@TLRDataSetControlFieldText_R,nil,'FieldText');
    RegisterPropertyHelper(@TLRDataSetControlFieldName_R,nil,'FieldName');
    RegisterPropertyHelper(@TLRDataSetControlDisplayLabel_R,nil,'DisplayLabel');
    RegisterPropertyHelper(@TLRDataSetControlDisplayWidth_R,nil,'DisplayWidth');
    RegisterPropertyHelper(@TLRDataSetControlActive_R,@TLRDataSetControlActive_W,'Active');
    RegisterPropertyHelper(@TLRDataSetControlEOF_R,nil,'EOF');
    RegisterPropertyHelper(@TLRDataSetControlRecordCount_R,nil,'RecordCount');
    RegisterPropertyHelper(@TLRDataSetControlFieldCount_R,nil,'FieldCount');
    RegisterPropertyHelper(@TLRDataSetControlFilter_R,@TLRDataSetControlFilter_W,'Filter');
    RegisterPropertyHelper(@TLRDataSetControlDataSource_R,@TLRDataSetControlDataSource_W,'DataSource');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectVisible_W(Self: TfrObject; const T: Boolean);
begin Self.Visible := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectVisible_R(Self: TfrObject; var T: Boolean);
begin T := Self.Visible; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectName_W(Self: TfrObject; const T: string);
begin Self.Name := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectName_R(Self: TfrObject; var T: string);
begin T := Self.Name; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectPSOnEnter_W(Self: TfrObject; const T: String);
begin Self.PSOnEnter := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectPSOnEnter_R(Self: TfrObject; var T: String);
begin T := Self.PSOnEnter; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectDesignOptions_R(Self: TfrObject; var T: TlrDesignOptions);
begin T := Self.DesignOptions; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectHeight_W(Self: TfrObject; const T: Integer);
begin Self.Height := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectHeight_R(Self: TfrObject; var T: Integer);
begin T := Self.Height; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectWidth_W(Self: TfrObject; const T: Integer);
begin Self.Width := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectWidth_R(Self: TfrObject; var T: Integer);
begin T := Self.Width; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectTop_W(Self: TfrObject; const T: Integer);
begin Self.Top := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectTop_R(Self: TfrObject; var T: Integer);
begin T := Self.Top; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectLeft_W(Self: TfrObject; const T: Integer);
begin Self.Left := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectLeft_R(Self: TfrObject; var T: Integer);
begin T := Self.Left; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectScript_W(Self: TfrObject; const T: TfrScriptStrings);
begin Self.Script := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectScript_R(Self: TfrObject; var T: TfrScriptStrings);
begin T := Self.Script; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectMemo_W(Self: TfrObject; const T: TfrMemoStrings);
begin Self.Memo := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectMemo_R(Self: TfrObject; var T: TfrMemoStrings);
begin T := Self.Memo; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectdy_W(Self: TfrObject; const T: Integer);
Begin Self.dy := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectdy_R(Self: TfrObject; var T: Integer);
Begin T := Self.dy; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectdx_W(Self: TfrObject; const T: Integer);
Begin Self.dx := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectdx_R(Self: TfrObject; var T: Integer);
Begin T := Self.dx; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjecty_W(Self: TfrObject; const T: Integer);
Begin Self.y := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjecty_R(Self: TfrObject; var T: Integer);
Begin T := Self.y; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectx_W(Self: TfrObject; const T: Integer);
Begin Self.x := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrObjectx_R(Self: TfrObject; var T: Integer);
Begin T := Self.x; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrObject(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrObject) do
  begin
    RegisterPropertyHelper(@TfrObjectx_R,@TfrObjectx_W,'x');
    RegisterPropertyHelper(@TfrObjecty_R,@TfrObjecty_W,'y');
    RegisterPropertyHelper(@TfrObjectdx_R,@TfrObjectdx_W,'dx');
    RegisterPropertyHelper(@TfrObjectdy_R,@TfrObjectdy_W,'dy');
    RegisterVirtualConstructor(@TfrObject.Create, 'Create');
    RegisterVirtualMethod(@TfrObject.BeginUpdate, 'BeginUpdate');
    RegisterVirtualMethod(@TfrObject.EndUpdate, 'EndUpdate');
    RegisterMethod(@TfrObject.CreateUniqueName, 'CreateUniqueName');
    RegisterPropertyHelper(@TfrObjectMemo_R,@TfrObjectMemo_W,'Memo');
    RegisterPropertyHelper(@TfrObjectScript_R,@TfrObjectScript_W,'Script');
    RegisterPropertyHelper(@TfrObjectLeft_R,@TfrObjectLeft_W,'Left');
    RegisterPropertyHelper(@TfrObjectTop_R,@TfrObjectTop_W,'Top');
    RegisterPropertyHelper(@TfrObjectWidth_R,@TfrObjectWidth_W,'Width');
    RegisterPropertyHelper(@TfrObjectHeight_R,@TfrObjectHeight_W,'Height');
    RegisterPropertyHelper(@TfrObjectDesignOptions_R,nil,'DesignOptions');
    RegisterPropertyHelper(@TfrObjectPSOnEnter_R,@TfrObjectPSOnEnter_W,'PSOnEnter');
    RegisterPropertyHelper(@TfrObjectName_R,@TfrObjectName_W,'Name');
    RegisterPropertyHelper(@TfrObjectVisible_R,@TfrObjectVisible_W,'Visible');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrViewHeight_W(Self: TfrView; const T: double);
begin Self.Height := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewHeight_R(Self: TfrView; var T: double);
begin T := Self.Height; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewWidth_W(Self: TfrView; const T: double);
begin Self.Width := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewWidth_R(Self: TfrView; var T: double);
begin T := Self.Width; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewURLInfo_W(Self: TfrView; const T: string);
begin Self.URLInfo := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewURLInfo_R(Self: TfrView; var T: string);
begin T := Self.URLInfo; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTag_W(Self: TfrView; const T: string);
begin Self.Tag := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTag_R(Self: TfrView; var T: string);
begin T := Self.Tag; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTop_W(Self: TfrView; const T: double);
begin Self.Top := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTop_R(Self: TfrView; var T: double);
begin T := Self.Top; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewLeft_W(Self: TfrView; const T: double);
begin Self.Left := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewLeft_R(Self: TfrView; var T: double);
begin T := Self.Left; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewGapY_W(Self: TfrView; const T: Integer);
begin Self.GapY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewGapY_R(Self: TfrView; var T: Integer);
begin T := Self.GapY; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewGapX_W(Self: TfrView; const T: Integer);
begin Self.GapX := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewGapX_R(Self: TfrView; var T: Integer);
begin T := Self.GapX; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFindHighlight_W(Self: TfrView; const T: boolean);
begin Self.FindHighlight := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFindHighlight_R(Self: TfrView; var T: boolean);
begin T := Self.FindHighlight; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewRestrictions_W(Self: TfrView; const T: TlrRestrictions);
begin Self.Restrictions := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewRestrictions_R(Self: TfrView; var T: TlrRestrictions);
begin T := Self.Restrictions; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewStreamMode_W(Self: TfrView; const T: TfrStreamMode);
begin Self.StreamMode := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewStreamMode_R(Self: TfrView; var T: TfrStreamMode);
begin T := Self.StreamMode; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFormatStr_W(Self: TfrView; const T: String);
begin Self.FormatStr := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFormatStr_R(Self: TfrView; var T: String);
begin T := Self.FormatStr; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFormat_W(Self: TfrView; const T: Integer);
begin Self.Format := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFormat_R(Self: TfrView; var T: Integer);
begin T := Self.Format; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameWidth_W(Self: TfrView; const T: Double);
begin Self.FrameWidth := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameWidth_R(Self: TfrView; var T: Double);
begin T := Self.FrameWidth; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameStyle_W(Self: TfrView; const T: TfrFrameStyle);
begin Self.FrameStyle := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameStyle_R(Self: TfrView; var T: TfrFrameStyle);
begin T := Self.FrameStyle; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameColor_W(Self: TfrView; const T: TColor);
begin Self.FrameColor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrameColor_R(Self: TfrView; var T: TColor);
begin T := Self.FrameColor; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrames_W(Self: TfrView; const T: TfrFrameBorders);
begin Self.Frames := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFrames_R(Self: TfrView; var T: TfrFrameBorders);
begin T := Self.Frames; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewStretched_W(Self: TfrView; const T: Boolean);
begin Self.Stretched := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewStretched_R(Self: TfrView; var T: Boolean);
begin T := Self.Stretched; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFillColor_W(Self: TfrView; const T: TColor);
begin Self.FillColor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFillColor_R(Self: TfrView; var T: TColor);
begin T := Self.FillColor; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewCanvas_W(Self: TfrView; const T: TCanvas);
begin Self.Canvas := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewCanvas_R(Self: TfrView; var T: TCanvas);
begin T := Self.Canvas; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewParentBandType_W(Self: TfrView; const T: TfrBandType);
Begin Self.ParentBandType := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewParentBandType_R(Self: TfrView; var T: TfrBandType);
Begin T := Self.ParentBandType; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewDRect_W(Self: TfrView; const T: TRect);
Begin Self.DRect := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewDRect_R(Self: TfrView; var T: TRect);
Begin T := Self.DRect; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFlags_W(Self: TfrView; const T: Word);
Begin Self.Flags := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewFlags_R(Self: TfrView; var T: Word);
Begin T := Self.Flags; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewIsPrinting_W(Self: TfrView; const T: Boolean);
Begin Self.IsPrinting := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewIsPrinting_R(Self: TfrView; var T: Boolean);
Begin T := Self.IsPrinting; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOffsY_W(Self: TfrView; const T: Integer);
Begin Self.OffsY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOffsY_R(Self: TfrView; var T: Integer);
Begin T := Self.OffsY; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOffsX_W(Self: TfrView; const T: Integer);
Begin Self.OffsX := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOffsX_R(Self: TfrView; var T: Integer);
Begin T := Self.OffsX; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewScaleY_W(Self: TfrView; const T: Double);
Begin Self.ScaleY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewScaleY_R(Self: TfrView; var T: Double);
Begin T := Self.ScaleY; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewScaleX_W(Self: TfrView; const T: Double);
Begin Self.ScaleX := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewScaleX_R(Self: TfrView; var T: Double);
Begin T := Self.ScaleX; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOriginalRect_W(Self: TfrView; const T: TRect);
Begin Self.OriginalRect := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewOriginalRect_R(Self: TfrView; var T: TRect);
Begin T := Self.OriginalRect; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewSelected_W(Self: TfrView; const T: Boolean);
Begin Self.Selected := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewSelected_R(Self: TfrView; var T: Boolean);
Begin T := Self.Selected; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTyp_W(Self: TfrView; const T: Byte);
Begin Self.Typ := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewTyp_R(Self: TfrView; var T: Byte);
Begin T := Self.Typ; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewID_W(Self: TfrView; const T: Integer);
Begin Self.ID := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewID_R(Self: TfrView; var T: Integer);
Begin T := Self.ID; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewParent_W(Self: TfrView; const T: TfrBand);
Begin Self.Parent := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrViewParent_R(Self: TfrView; var T: TfrBand);
Begin T := Self.Parent; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrView) do
  begin
    RegisterPropertyHelper(@TfrViewParent_R,@TfrViewParent_W,'Parent');
    RegisterPropertyHelper(@TfrViewID_R,@TfrViewID_W,'ID');
    RegisterPropertyHelper(@TfrViewTyp_R,@TfrViewTyp_W,'Typ');
    RegisterPropertyHelper(@TfrViewSelected_R,@TfrViewSelected_W,'Selected');
    RegisterPropertyHelper(@TfrViewOriginalRect_R,@TfrViewOriginalRect_W,'OriginalRect');
    RegisterPropertyHelper(@TfrViewScaleX_R,@TfrViewScaleX_W,'ScaleX');
    RegisterPropertyHelper(@TfrViewScaleY_R,@TfrViewScaleY_W,'ScaleY');
    RegisterPropertyHelper(@TfrViewOffsX_R,@TfrViewOffsX_W,'OffsX');
    RegisterPropertyHelper(@TfrViewOffsY_R,@TfrViewOffsY_W,'OffsY');
    RegisterPropertyHelper(@TfrViewIsPrinting_R,@TfrViewIsPrinting_W,'IsPrinting');
    RegisterPropertyHelper(@TfrViewFlags_R,@TfrViewFlags_W,'Flags');
    RegisterPropertyHelper(@TfrViewDRect_R,@TfrViewDRect_W,'DRect');
    RegisterPropertyHelper(@TfrViewParentBandType_R,@TfrViewParentBandType_W,'ParentBandType');
    RegisterMethod(@TfrView.SetBounds, 'SetBounds');
    RegisterPropertyHelper(@TfrViewCanvas_R,@TfrViewCanvas_W,'Canvas');
    RegisterPropertyHelper(@TfrViewFillColor_R,@TfrViewFillColor_W,'FillColor');
    RegisterPropertyHelper(@TfrViewStretched_R,@TfrViewStretched_W,'Stretched');
    RegisterPropertyHelper(@TfrViewFrames_R,@TfrViewFrames_W,'Frames');
    RegisterPropertyHelper(@TfrViewFrameColor_R,@TfrViewFrameColor_W,'FrameColor');
    RegisterPropertyHelper(@TfrViewFrameStyle_R,@TfrViewFrameStyle_W,'FrameStyle');
    RegisterPropertyHelper(@TfrViewFrameWidth_R,@TfrViewFrameWidth_W,'FrameWidth');
    RegisterPropertyHelper(@TfrViewFormat_R,@TfrViewFormat_W,'Format');
    RegisterPropertyHelper(@TfrViewFormatStr_R,@TfrViewFormatStr_W,'FormatStr');
    RegisterPropertyHelper(@TfrViewStreamMode_R,@TfrViewStreamMode_W,'StreamMode');
    RegisterPropertyHelper(@TfrViewRestrictions_R,@TfrViewRestrictions_W,'Restrictions');
    RegisterPropertyHelper(@TfrViewFindHighlight_R,@TfrViewFindHighlight_W,'FindHighlight');
    RegisterPropertyHelper(@TfrViewGapX_R,@TfrViewGapX_W,'GapX');
    RegisterPropertyHelper(@TfrViewGapY_R,@TfrViewGapY_W,'GapY');
    RegisterPropertyHelper(@TfrViewLeft_R,@TfrViewLeft_W,'Left');
    RegisterPropertyHelper(@TfrViewTop_R,@TfrViewTop_W,'Top');
    RegisterPropertyHelper(@TfrViewTag_R,@TfrViewTag_W,'Tag');
    RegisterPropertyHelper(@TfrViewURLInfo_R,@TfrViewURLInfo_W,'URLInfo');
    RegisterPropertyHelper(@TfrViewWidth_R,@TfrViewWidth_W,'Width');
    RegisterPropertyHelper(@TfrViewHeight_R,@TfrViewHeight_W,'Height');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrStretcheable(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrStretcheable) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrControl(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrControl) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrNonVisualControl(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrNonVisualControl) do
  begin
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLineSpacing_W(Self: TfrCustomMemoView; const T: integer);
begin Self.LineSpacing := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLineSpacing_R(Self: TfrCustomMemoView; var T: integer);
begin T := Self.LineSpacing; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewParagraphGap_W(Self: TfrCustomMemoView; const T: integer);
begin Self.ParagraphGap := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewParagraphGap_R(Self: TfrCustomMemoView; var T: integer);
begin T := Self.ParagraphGap; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnMouseLeave_W(Self: TfrCustomMemoView; const T: TfrScriptStrings);
begin Self.OnMouseLeave := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnMouseLeave_R(Self: TfrCustomMemoView; var T: TfrScriptStrings);
begin T := Self.OnMouseLeave; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnMouseEnter_W(Self: TfrCustomMemoView; const T: TfrScriptStrings);
begin Self.OnMouseEnter := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnMouseEnter_R(Self: TfrCustomMemoView; var T: TfrScriptStrings);
begin T := Self.OnMouseEnter; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnClick_W(Self: TfrCustomMemoView; const T: TfrScriptStrings);
begin Self.OnClick := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewOnClick_R(Self: TfrCustomMemoView; var T: TfrScriptStrings);
begin T := Self.OnClick; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHideZeroValues_W(Self: TfrCustomMemoView; const T: Boolean);
begin Self.HideZeroValues := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHideZeroValues_R(Self: TfrCustomMemoView; var T: Boolean);
begin T := Self.HideZeroValues; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHideDuplicates_W(Self: TfrCustomMemoView; const T: Boolean);
begin Self.HideDuplicates := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHideDuplicates_R(Self: TfrCustomMemoView; var T: Boolean);
begin T := Self.HideDuplicates; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAutoSize_W(Self: TfrCustomMemoView; const T: Boolean);
begin Self.AutoSize := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAutoSize_R(Self: TfrCustomMemoView; var T: Boolean);
begin T := Self.AutoSize; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewWordWrap_W(Self: TfrCustomMemoView; const T: Boolean);
begin Self.WordWrap := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewWordWrap_R(Self: TfrCustomMemoView; var T: Boolean);
begin T := Self.WordWrap; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewWordBreak_W(Self: TfrCustomMemoView; const T: Boolean);
begin Self.WordBreak := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewWordBreak_R(Self: TfrCustomMemoView; var T: Boolean);
begin T := Self.WordBreak; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAngle_W(Self: TfrCustomMemoView; const T: Byte);
begin Self.Angle := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAngle_R(Self: TfrCustomMemoView; var T: Byte);
begin T := Self.Angle; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLayout_W(Self: TfrCustomMemoView; const T: TTextLayout);
begin Self.Layout := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLayout_R(Self: TfrCustomMemoView; var T: TTextLayout);
begin T := Self.Layout; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAlignment_W(Self: TfrCustomMemoView; const T: TAlignment);
begin Self.Alignment := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAlignment_R(Self: TfrCustomMemoView; var T: TAlignment);
begin T := Self.Alignment; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewFont_W(Self: TfrCustomMemoView; const T: TFont);
begin Self.Font := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewFont_R(Self: TfrCustomMemoView; var T: TFont);
begin T := Self.Font; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewDetailReport_W(Self: TfrCustomMemoView; const T: string);
begin Self.DetailReport := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewDetailReport_R(Self: TfrCustomMemoView; var T: string);
begin T := Self.DetailReport; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewCursor_W(Self: TfrCustomMemoView; const T: TCursor);
begin Self.Cursor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewCursor_R(Self: TfrCustomMemoView; var T: TCursor);
begin T := Self.Cursor; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewJustify_W(Self: TfrCustomMemoView; const T: boolean);
begin Self.Justify := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewJustify_R(Self: TfrCustomMemoView; var T: boolean);
begin T := Self.Justify; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewFirstLine_W(Self: TfrCustomMemoView; const T: boolean);
Begin Self.FirstLine := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewFirstLine_R(Self: TfrCustomMemoView; var T: boolean);
Begin T := Self.FirstLine; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLastLine_W(Self: TfrCustomMemoView; const T: boolean);
Begin Self.LastLine := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewLastLine_R(Self: TfrCustomMemoView; var T: boolean);
Begin T := Self.LastLine; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewCharacterSpacing_W(Self: TfrCustomMemoView; const T: Integer);
Begin Self.CharacterSpacing := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewCharacterSpacing_R(Self: TfrCustomMemoView; var T: Integer);
Begin T := Self.CharacterSpacing; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHighlightStr_W(Self: TfrCustomMemoView; const T: String);
Begin Self.HighlightStr := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHighlightStr_R(Self: TfrCustomMemoView; var T: String);
Begin T := Self.HighlightStr; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHighlight_W(Self: TfrCustomMemoView; const T: TfrHighlightAttr);
Begin Self.Highlight := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewHighlight_R(Self: TfrCustomMemoView; var T: TfrHighlightAttr);
Begin T := Self.Highlight; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAdjust_W(Self: TfrCustomMemoView; const T: Integer);
Begin Self.Adjust := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrCustomMemoViewAdjust_R(Self: TfrCustomMemoView; var T: Integer);
Begin T := Self.Adjust; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrCustomMemoView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrCustomMemoView) do
  begin
    RegisterPropertyHelper(@TfrCustomMemoViewAdjust_R,@TfrCustomMemoViewAdjust_W,'Adjust');
    RegisterPropertyHelper(@TfrCustomMemoViewHighlight_R,@TfrCustomMemoViewHighlight_W,'Highlight');
    RegisterPropertyHelper(@TfrCustomMemoViewHighlightStr_R,@TfrCustomMemoViewHighlightStr_W,'HighlightStr');
    RegisterPropertyHelper(@TfrCustomMemoViewCharacterSpacing_R,@TfrCustomMemoViewCharacterSpacing_W,'CharacterSpacing');
    RegisterPropertyHelper(@TfrCustomMemoViewLastLine_R,@TfrCustomMemoViewLastLine_W,'LastLine');
    RegisterPropertyHelper(@TfrCustomMemoViewFirstLine_R,@TfrCustomMemoViewFirstLine_W,'FirstLine');
    RegisterMethod(@TfrCustomMemoView.MonitorFontChanges, 'MonitorFontChanges');
    RegisterPropertyHelper(@TfrCustomMemoViewJustify_R,@TfrCustomMemoViewJustify_W,'Justify');
    RegisterPropertyHelper(@TfrCustomMemoViewCursor_R,@TfrCustomMemoViewCursor_W,'Cursor');
    RegisterPropertyHelper(@TfrCustomMemoViewDetailReport_R,@TfrCustomMemoViewDetailReport_W,'DetailReport');
    RegisterPropertyHelper(@TfrCustomMemoViewFont_R,@TfrCustomMemoViewFont_W,'Font');
    RegisterPropertyHelper(@TfrCustomMemoViewAlignment_R,@TfrCustomMemoViewAlignment_W,'Alignment');
    RegisterPropertyHelper(@TfrCustomMemoViewLayout_R,@TfrCustomMemoViewLayout_W,'Layout');
    RegisterPropertyHelper(@TfrCustomMemoViewAngle_R,@TfrCustomMemoViewAngle_W,'Angle');
    RegisterPropertyHelper(@TfrCustomMemoViewWordBreak_R,@TfrCustomMemoViewWordBreak_W,'WordBreak');
    RegisterPropertyHelper(@TfrCustomMemoViewWordWrap_R,@TfrCustomMemoViewWordWrap_W,'WordWrap');
    RegisterPropertyHelper(@TfrCustomMemoViewAutoSize_R,@TfrCustomMemoViewAutoSize_W,'AutoSize');
    RegisterPropertyHelper(@TfrCustomMemoViewHideDuplicates_R,@TfrCustomMemoViewHideDuplicates_W,'HideDuplicates');
    RegisterPropertyHelper(@TfrCustomMemoViewHideZeroValues_R,@TfrCustomMemoViewHideZeroValues_W,'HideZeroValues');
    RegisterPropertyHelper(@TfrCustomMemoViewOnClick_R,@TfrCustomMemoViewOnClick_W,'OnClick');
    RegisterPropertyHelper(@TfrCustomMemoViewOnMouseEnter_R,@TfrCustomMemoViewOnMouseEnter_W,'OnMouseEnter');
    RegisterPropertyHelper(@TfrCustomMemoViewOnMouseLeave_R,@TfrCustomMemoViewOnMouseLeave_W,'OnMouseLeave');
    RegisterPropertyHelper(@TfrCustomMemoViewParagraphGap_R,@TfrCustomMemoViewParagraphGap_W,'ParagraphGap');
    RegisterPropertyHelper(@TfrCustomMemoViewLineSpacing_R,@TfrCustomMemoViewLineSpacing_W,'LineSpacing');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrMemoView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrMemoView) do
  begin
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrBandViewBandType_W(Self: TfrBandView; const T: TfrBandType);
begin Self.BandType := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewBandType_R(Self: TfrBandView; var T: TfrBandType);
begin T := Self.BandType; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewChild_W(Self: TfrBandView; const T: String);
begin Self.Child := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewChild_R(Self: TfrBandView; var T: String);
begin T := Self.Child; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewGroupCondition_W(Self: TfrBandView; const T: String);
begin Self.GroupCondition := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewGroupCondition_R(Self: TfrBandView; var T: String);
begin T := Self.GroupCondition; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewDataSet_W(Self: TfrBandView; const T: String);
begin Self.DataSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandViewDataSet_R(Self: TfrBandView; var T: String);
begin T := Self.DataSet; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrBandView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrBandView) do
  begin
    RegisterPropertyHelper(@TfrBandViewDataSet_R,@TfrBandViewDataSet_W,'DataSet');
    RegisterPropertyHelper(@TfrBandViewGroupCondition_R,@TfrBandViewGroupCondition_W,'GroupCondition');
    RegisterPropertyHelper(@TfrBandViewChild_R,@TfrBandViewChild_W,'Child');
    RegisterPropertyHelper(@TfrBandViewBandType_R,@TfrBandViewBandType_W,'BandType');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrSubReportViewSubPage_W(Self: TfrSubReportView; const T: TfrPage);
begin Self.SubPage := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrSubReportViewSubPage_R(Self: TfrSubReportView; var T: TfrPage);
begin T := Self.SubPage; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrSubReportView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrSubReportView) do
  begin
    RegisterPropertyHelper(@TfrSubReportViewSubPage_R,@TfrSubReportViewSubPage_W,'SubPage');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrPictureViewSharedName_W(Self: TfrPictureView; const T: string);
begin Self.SharedName := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewSharedName_R(Self: TfrPictureView; var T: string);
begin T := Self.SharedName; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewCentered_W(Self: TfrPictureView; const T: boolean);
begin Self.Centered := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewCentered_R(Self: TfrPictureView; var T: boolean);
begin T := Self.Centered; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewKeepAspect_W(Self: TfrPictureView; const T: boolean);
begin Self.KeepAspect := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewKeepAspect_R(Self: TfrPictureView; var T: boolean);
begin T := Self.KeepAspect; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewPicture_W(Self: TfrPictureView; const T: TPicture);
begin Self.Picture := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPictureViewPicture_R(Self: TfrPictureView; var T: TPicture);
begin T := Self.Picture; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrPictureView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrPictureView) do
  begin
    RegisterPropertyHelper(@TfrPictureViewPicture_R,@TfrPictureViewPicture_W,'Picture');
    RegisterPropertyHelper(@TfrPictureViewKeepAspect_R,@TfrPictureViewKeepAspect_W,'KeepAspect');
    RegisterPropertyHelper(@TfrPictureViewCentered_R,@TfrPictureViewCentered_W,'Centered');
    RegisterPropertyHelper(@TfrPictureViewSharedName_R,@TfrPictureViewSharedName_W,'SharedName');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrLineView(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrLineView) do
  begin
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrRectBottom_W(Self: TfrRect; const T: Integer);
begin Self.Bottom := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectBottom_R(Self: TfrRect; var T: Integer);
begin T := Self.Bottom; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectRight_W(Self: TfrRect; const T: Integer);
begin Self.Right := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectRight_R(Self: TfrRect; var T: Integer);
begin T := Self.Right; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectTop_W(Self: TfrRect; const T: Integer);
begin Self.Top := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectTop_R(Self: TfrRect; var T: Integer);
begin T := Self.Top; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectLeft_W(Self: TfrRect; const T: Integer);
begin Self.Left := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectLeft_R(Self: TfrRect; var T: Integer);
begin T := Self.Left; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectAsRect_W(Self: TfrRect; const T: TRect);
begin Self.AsRect := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrRectAsRect_R(Self: TfrRect; var T: TRect);
begin T := Self.AsRect; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrRect(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrRect) do
  begin
    RegisterPropertyHelper(@TfrRectAsRect_R,@TfrRectAsRect_W,'AsRect');
    RegisterPropertyHelper(@TfrRectLeft_R,@TfrRectLeft_W,'Left');
    RegisterPropertyHelper(@TfrRectTop_R,@TfrRectTop_W,'Top');
    RegisterPropertyHelper(@TfrRectRight_R,@TfrRectRight_W,'Right');
    RegisterPropertyHelper(@TfrRectBottom_R,@TfrRectBottom_W,'Bottom');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrBandName_R(Self: TfrBand; var T: string);
begin T := Self.Name; end;

(*----------------------------------------------------------------------------*)
Function TfrBandCreate2_P(Self: TClass; CreateNewInstance: Boolean;  ATyp : TfrBandType; AParent : TfrPage):TObject;
Begin Result := TfrBand.Create(ATyp, AParent); END;

(*----------------------------------------------------------------------------*)
procedure TfrBandForceNewColumn_W(Self: TfrBand; const T: Boolean);
Begin Self.ForceNewColumn := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandForceNewColumn_R(Self: TfrBand; var T: Boolean);
Begin T := Self.ForceNewColumn; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandForceNewPage_W(Self: TfrBand; const T: Boolean);
Begin Self.ForceNewPage := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandForceNewPage_R(Self: TfrBand; var T: Boolean);
Begin T := Self.ForceNewPage; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandGroupCondition_W(Self: TfrBand; const T: String);
Begin Self.GroupCondition := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandGroupCondition_R(Self: TfrBand; var T: String);
Begin T := Self.GroupCondition; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandIsVirtualVCDS_W(Self: TfrBand; const T: Boolean);
Begin Self.IsVirtualVCDS := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandIsVirtualVCDS_R(Self: TfrBand; var T: Boolean);
Begin T := Self.IsVirtualVCDS; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandVCDataSet_W(Self: TfrBand; const T: TfrDataSet);
Begin Self.VCDataSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandVCDataSet_R(Self: TfrBand; var T: TfrDataSet);
Begin T := Self.VCDataSet; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandIsVirtualDS_W(Self: TfrBand; const T: Boolean);
Begin Self.IsVirtualDS := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandIsVirtualDS_R(Self: TfrBand; var T: Boolean);
Begin T := Self.IsVirtualDS; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandDataSet_W(Self: TfrBand; const T: TfrDataSet);
Begin Self.DataSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandDataSet_R(Self: TfrBand; var T: TfrDataSet);
Begin T := Self.DataSet; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandObjects_W(Self: TfrBand; const T: TFpList);
Begin Self.Objects := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandObjects_R(Self: TfrBand; var T: TFpList);
Begin T := Self.Objects; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPrintChildIfNotVisible_W(Self: TfrBand; const T: Boolean);
Begin Self.PrintChildIfNotVisible := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPrintChildIfNotVisible_R(Self: TfrBand; var T: Boolean);
Begin T := Self.PrintChildIfNotVisible; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPageBreak_W(Self: TfrBand; const T: Boolean);
Begin Self.PageBreak := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPageBreak_R(Self: TfrBand; var T: Boolean);
Begin T := Self.PageBreak; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandStretched_W(Self: TfrBand; const T: Boolean);
Begin Self.Stretched := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandStretched_R(Self: TfrBand; var T: Boolean);
Begin T := Self.Stretched; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandNewPageAfter_W(Self: TfrBand; const T: Boolean);
Begin Self.NewPageAfter := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandNewPageAfter_R(Self: TfrBand; var T: Boolean);
Begin T := Self.NewPageAfter; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPrintIfSubsetEmpty_W(Self: TfrBand; const T: Boolean);
Begin Self.PrintIfSubsetEmpty := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandPrintIfSubsetEmpty_R(Self: TfrBand; var T: Boolean);
Begin T := Self.PrintIfSubsetEmpty; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandTyp_W(Self: TfrBand; const T: TfrBandType);
Begin Self.Typ := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandTyp_R(Self: TfrBand; var T: TfrBandType);
Begin T := Self.Typ; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandMaxDY_W(Self: TfrBand; const T: Integer);
Begin Self.MaxDY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandMaxDY_R(Self: TfrBand; var T: Integer);
Begin T := Self.MaxDY; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandEOFReached_W(Self: TfrBand; const T: Boolean);
Begin Self.EOFReached := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrBandEOFReached_R(Self: TfrBand; var T: Boolean);
Begin T := Self.EOFReached; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrBand(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrBand) do
  begin
    RegisterPropertyHelper(@TfrBandEOFReached_R,@TfrBandEOFReached_W,'EOFReached');
    RegisterPropertyHelper(@TfrBandMaxDY_R,@TfrBandMaxDY_W,'MaxDY');
    RegisterPropertyHelper(@TfrBandTyp_R,@TfrBandTyp_W,'Typ');
    RegisterPropertyHelper(@TfrBandPrintIfSubsetEmpty_R,@TfrBandPrintIfSubsetEmpty_W,'PrintIfSubsetEmpty');
    RegisterPropertyHelper(@TfrBandNewPageAfter_R,@TfrBandNewPageAfter_W,'NewPageAfter');
    RegisterPropertyHelper(@TfrBandStretched_R,@TfrBandStretched_W,'Stretched');
    RegisterPropertyHelper(@TfrBandPageBreak_R,@TfrBandPageBreak_W,'PageBreak');
    RegisterPropertyHelper(@TfrBandPrintChildIfNotVisible_R,@TfrBandPrintChildIfNotVisible_W,'PrintChildIfNotVisible');
    RegisterPropertyHelper(@TfrBandObjects_R,@TfrBandObjects_W,'Objects');
    RegisterPropertyHelper(@TfrBandDataSet_R,@TfrBandDataSet_W,'DataSet');
    RegisterPropertyHelper(@TfrBandIsVirtualDS_R,@TfrBandIsVirtualDS_W,'IsVirtualDS');
    RegisterPropertyHelper(@TfrBandVCDataSet_R,@TfrBandVCDataSet_W,'VCDataSet');
    RegisterPropertyHelper(@TfrBandIsVirtualVCDS_R,@TfrBandIsVirtualVCDS_W,'IsVirtualVCDS');
    RegisterPropertyHelper(@TfrBandGroupCondition_R,@TfrBandGroupCondition_W,'GroupCondition');
    RegisterPropertyHelper(@TfrBandForceNewPage_R,@TfrBandForceNewPage_W,'ForceNewPage');
    RegisterPropertyHelper(@TfrBandForceNewColumn_R,@TfrBandForceNewColumn_W,'ForceNewColumn');
    RegisterConstructor(@TfrBandCreate2_P, 'Create2');
    RegisterMethod(@TfrBand.IsDataBand, 'IsDataBand');
    RegisterPropertyHelper(@TfrBandName_R,nil,'Name');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrValueDSet_W(Self: TfrValue; const T: TfrTDataSet);
Begin Self.DSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueDSet_R(Self: TfrValue; var T: TfrTDataSet);
Begin T := Self.DSet; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueField_W(Self: TfrValue; const T: String);
Begin Self.Field := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueField_R(Self: TfrValue; var T: String);
Begin T := Self.Field; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueDataSet_W(Self: TfrValue; const T: String);
Begin Self.DataSet := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueDataSet_R(Self: TfrValue; var T: String);
Begin T := Self.DataSet; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueOtherKind_W(Self: TfrValue; const T: Integer);
Begin Self.OtherKind := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueOtherKind_R(Self: TfrValue; var T: Integer);
Begin T := Self.OtherKind; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueTyp_W(Self: TfrValue; const T: TfrValueType);
Begin Self.Typ := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValueTyp_R(Self: TfrValue; var T: TfrValueType);
Begin T := Self.Typ; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrValue(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrValue) do
  begin
    RegisterPropertyHelper(@TfrValueTyp_R,@TfrValueTyp_W,'Typ');
    RegisterPropertyHelper(@TfrValueOtherKind_R,@TfrValueOtherKind_W,'OtherKind');
    RegisterPropertyHelper(@TfrValueDataSet_R,@TfrValueDataSet_W,'DataSet');
    RegisterPropertyHelper(@TfrValueField_R,@TfrValueField_W,'Field');
    RegisterPropertyHelper(@TfrValueDSet_R,@TfrValueDSet_W,'DSet');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrValuesObjects_R(Self: TfrValues; var T: TfrValue; const t1: Integer);
begin T := Self.Objects[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TfrValuesItems_W(Self: TfrValues; const T: TStringList);
begin Self.Items := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrValuesItems_R(Self: TfrValues; var T: TStringList);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrValues(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrValues) do
  begin
    RegisterVirtualConstructor(@TfrValues.Create, 'Create');
    RegisterMethod(@TfrValues.AddValue, 'AddValue');
    RegisterMethod(@TfrValues.FindVariable, 'FindVariable');
    RegisterMethod(@TfrValues.Clear, 'Clear');
    RegisterPropertyHelper(@TfrValuesItems_R,@TfrValuesItems_W,'Items');
    RegisterPropertyHelper(@TfrValuesObjects_R,nil,'Objects');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrPagePageIndex_W(Self: TfrPage; const T: integer);
begin Self.PageIndex := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagePageIndex_R(Self: TfrPage; var T: integer);
begin T := Self.PageIndex; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLastBandType_W(Self: TfrPage; const T: TfrBandType);
begin Self.LastBandType := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLastBandType_R(Self: TfrPage; var T: TfrBandType);
begin T := Self.LastBandType; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageRowStarted_W(Self: TfrPage; const T: boolean);
begin Self.RowStarted := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageRowStarted_R(Self: TfrPage; var T: boolean);
begin T := Self.RowStarted; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLastRowHeight_W(Self: TfrPage; const T: Integer);
begin Self.LastRowHeight := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLastRowHeight_R(Self: TfrPage; var T: Integer);
begin T := Self.LastRowHeight; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLayoutOrder_W(Self: TfrPage; const T: TLayoutOrder);
begin Self.LayoutOrder := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageLayoutOrder_R(Self: TfrPage; var T: TLayoutOrder);
begin T := Self.LayoutOrder; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageOrientation_W(Self: TfrPage; const T: TPrinterOrientation);
begin Self.Orientation := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageOrientation_R(Self: TfrPage; var T: TPrinterOrientation);
begin T := Self.Orientation; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagePrintToPrevPage_W(Self: TfrPage; const T: Boolean);
begin Self.PrintToPrevPage := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagePrintToPrevPage_R(Self: TfrPage; var T: Boolean);
begin T := Self.PrintToPrevPage; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageMargins_W(Self: TfrPage; const T: TfrRect);
begin Self.Margins := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageMargins_R(Self: TfrPage; var T: TfrRect);
begin T := Self.Margins; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageUseMargins_W(Self: TfrPage; const T: Boolean);
begin Self.UseMargins := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageUseMargins_R(Self: TfrPage; var T: Boolean);
begin T := Self.UseMargins; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColGap_W(Self: TfrPage; const T: Integer);
begin Self.ColGap := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColGap_R(Self: TfrPage; var T: Integer);
begin T := Self.ColGap; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColWidth_W(Self: TfrPage; const T: Integer);
begin Self.ColWidth := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColWidth_R(Self: TfrPage; var T: Integer);
begin T := Self.ColWidth; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColCount_W(Self: TfrPage; const T: Integer);
begin Self.ColCount := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageColCount_R(Self: TfrPage; var T: Integer);
begin T := Self.ColCount; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageCurBottomY_W(Self: TfrPage; const T: Integer);
Begin Self.CurBottomY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageCurBottomY_R(Self: TfrPage; var T: Integer);
Begin T := Self.CurBottomY; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageCurY_W(Self: TfrPage; const T: Integer);
Begin Self.CurY := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageCurY_R(Self: TfrPage; var T: Integer);
Begin T := Self.CurY; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageRTObjects_W(Self: TfrPage; const T: TFpList);
Begin Self.RTObjects := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageRTObjects_R(Self: TfrPage; var T: TFpList);
Begin T := Self.RTObjects; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageObjects_W(Self: TfrPage; const T: TFpList);
Begin Self.Objects := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageObjects_R(Self: TfrPage; var T: TFpList);
Begin T := Self.Objects; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagePrnInfo_W(Self: TfrPage; const T: TfrPrnInfo);
Begin Self.PrnInfo := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagePrnInfo_R(Self: TfrPage; var T: TfrPrnInfo);
Begin T := Self.PrnInfo; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagepgSize_W(Self: TfrPage; const T: Integer);
Begin Self.pgSize := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagepgSize_R(Self: TfrPage; var T: Integer);
Begin T := Self.pgSize; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrPage(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrPage) do
  begin
    RegisterPropertyHelper(@TfrPagepgSize_R,@TfrPagepgSize_W,'pgSize');
    RegisterPropertyHelper(@TfrPagePrnInfo_R,@TfrPagePrnInfo_W,'PrnInfo');
    RegisterPropertyHelper(@TfrPageObjects_R,@TfrPageObjects_W,'Objects');
    RegisterPropertyHelper(@TfrPageRTObjects_R,@TfrPageRTObjects_W,'RTObjects');
    RegisterPropertyHelper(@TfrPageCurY_R,@TfrPageCurY_W,'CurY');
    RegisterPropertyHelper(@TfrPageCurBottomY_R,@TfrPageCurBottomY_W,'CurBottomY');
    RegisterConstructor(@TfrPage.Create, 'Create');
    RegisterVirtualConstructor(@TfrPage.CreatePage, 'CreatePage');
    RegisterMethod(@TfrPage.TopMargin, 'TopMargin');
    RegisterMethod(@TfrPage.BottomMargin, 'BottomMargin');
    RegisterMethod(@TfrPage.LeftMargin, 'LeftMargin');
    RegisterMethod(@TfrPage.RightMargin, 'RightMargin');
    RegisterMethod(@TfrPage.Add, 'Add');
    RegisterMethod(@TfrPage.Clear, 'Clear');
    RegisterMethod(@TfrPage.Delete, 'Delete');
    RegisterMethod(@TfrPage.FindObjectByID, 'FindObjectByID');
    RegisterMethod(@TfrPage.FindObject, 'FindObject');
    RegisterMethod(@TfrPage.FindRTObject, 'FindRTObject');
    RegisterMethod(@TfrPage.ChangePaper, 'ChangePaper');
    RegisterMethod(@TfrPage.ShowBandByName, 'ShowBandByName');
    RegisterMethod(@TfrPage.ShowBandByType, 'ShowBandByType');
    RegisterMethod(@TfrPage.NewPage, 'NewPage');
    RegisterMethod(@TfrPage.NewColumn, 'NewColumn');
    RegisterMethod(@TfrPage.NextColumn, 'NextColumn');
    RegisterMethod(@TfrPage.RowsLayout, 'RowsLayout');
    RegisterMethod(@TfrPage.StartColumn, 'StartColumn');
    RegisterMethod(@TfrPage.StartRowsLayoutNonDataBand, 'StartRowsLayoutNonDataBand');
    RegisterMethod(@TfrPage.AdvanceRow, 'AdvanceRow');
    RegisterPropertyHelper(@TfrPageColCount_R,@TfrPageColCount_W,'ColCount');
    RegisterPropertyHelper(@TfrPageColWidth_R,@TfrPageColWidth_W,'ColWidth');
    RegisterPropertyHelper(@TfrPageColGap_R,@TfrPageColGap_W,'ColGap');
    RegisterPropertyHelper(@TfrPageUseMargins_R,@TfrPageUseMargins_W,'UseMargins');
    RegisterPropertyHelper(@TfrPageMargins_R,@TfrPageMargins_W,'Margins');
    RegisterPropertyHelper(@TfrPagePrintToPrevPage_R,@TfrPagePrintToPrevPage_W,'PrintToPrevPage');
    RegisterPropertyHelper(@TfrPageOrientation_R,@TfrPageOrientation_W,'Orientation');
    RegisterPropertyHelper(@TfrPageLayoutOrder_R,@TfrPageLayoutOrder_W,'LayoutOrder');
    RegisterPropertyHelper(@TfrPageLastRowHeight_R,@TfrPageLastRowHeight_W,'LastRowHeight');
    RegisterPropertyHelper(@TfrPageRowStarted_R,@TfrPageRowStarted_W,'RowStarted');
    RegisterPropertyHelper(@TfrPageLastBandType_R,@TfrPageLastBandType_W,'LastBandType');
    RegisterPropertyHelper(@TfrPagePageIndex_R,@TfrPagePageIndex_W,'PageIndex');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrPageReport(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrPageReport) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure TfrPageDialogCaption_W(Self: TfrPageDialog; const T: string);
begin Self.Caption := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageDialogCaption_R(Self: TfrPageDialog; var T: string);
begin T := Self.Caption; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageDialogModalResult_W(Self: TfrPageDialog; const T: Integer);
begin Self.ModalResult := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageDialogModalResult_R(Self: TfrPageDialog; var T: Integer);
begin T := Self.ModalResult; end;

(*----------------------------------------------------------------------------*)
procedure TfrPageDialogForm_R(Self: TfrPageDialog; var T: TfrDialogForm);
begin T := Self.Form; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrPageDialog(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrPageDialog) do
  begin
    RegisterMethod(@TfrPageDialog.ShowModal, 'ShowModal');
    RegisterPropertyHelper(@TfrPageDialogForm_R,nil,'Form');
    RegisterPropertyHelper(@TfrPageDialogCaption_R,@TfrPageDialogCaption_W,'Caption');
    RegisterPropertyHelper(@TfrPageDialogModalResult_R,@TfrPageDialogModalResult_W,'ModalResult');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrPagesCount_R(Self: TfrPages; var T: Integer);
begin T := Self.Count; end;

(*----------------------------------------------------------------------------*)
procedure TfrPagesPages_R(Self: TfrPages; var T: TfrPage; const t1: Integer);
begin T := Self.Pages[t1]; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrPages(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrPages) do
  begin
    RegisterConstructor(@TfrPages.Create, 'Create');
    RegisterMethod(@TfrPages.Clear, 'Clear');
    RegisterMethod(@TfrPages.Add, 'Add');
    RegisterMethod(@TfrPages.Delete, 'Delete');
    RegisterMethod(@TfrPages.Move, 'Move');
    RegisterMethod(@TfrPages.PageByName, 'PageByName');
    RegisterPropertyHelper(@TfrPagesPages_R,nil,'Pages');
    RegisterPropertyHelper(@TfrPagesCount_R,nil,'Count');
  end;
end;


(*----------------------------------------------------------------------------*)
procedure TfrReportPasScript_W(Self: TfrReport; const T: TStringList);
begin Self.PasScript := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPasScript_R(Self: TfrReport; var T: TStrings);
begin T := Self.PasScript; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportTitle_W(Self: TfrReport; const T: String);
begin Self.Title := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportTitle_R(Self: TfrReport; var T: String);
begin T := Self.Title; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDataType_W(Self: TfrReport; const T: TfrDataType);
begin Self.DataType := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDataType_R(Self: TfrReport; var T: TfrDataType);
begin T := Self.DataType; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportStoreInForm_W(Self: TfrReport; const T: Boolean);
begin Self.StoreInForm := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportStoreInForm_R(Self: TfrReport; var T: Boolean);
begin T := Self.StoreInForm; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportShowProgress_W(Self: TfrReport; const T: Boolean);
begin Self.ShowProgress := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportShowProgress_R(Self: TfrReport; var T: Boolean);
begin T := Self.ShowProgress; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportType_W(Self: TfrReport; const T: TfrReportType);
begin Self.ReportType := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportType_R(Self: TfrReport; var T: TfrReportType);
begin T := Self.ReportType; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportRebuildPrinter_W(Self: TfrReport; const T: boolean);
begin Self.RebuildPrinter := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportRebuildPrinter_R(Self: TfrReport; var T: boolean);
begin T := Self.RebuildPrinter; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPreviewButtons_W(Self: TfrReport; const T: TfrPreviewButtons);
begin Self.PreviewButtons := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPreviewButtons_R(Self: TfrReport; var T: TfrPreviewButtons);
begin T := Self.PreviewButtons; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPreview_W(Self: TfrReport; const T: TfrPreview);
begin Self.Preview := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPreview_R(Self: TfrReport; var T: TfrPreview);
begin T := Self.Preview; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportOptions_W(Self: TfrReport; const T: TfrReportOptions);
begin Self.Options := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportOptions_R(Self: TfrReport; var T: TfrReportOptions);
begin T := Self.Options; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportModifyPrepared_W(Self: TfrReport; const T: Boolean);
begin Self.ModifyPrepared := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportModifyPrepared_R(Self: TfrReport; var T: Boolean);
begin T := Self.ModifyPrepared; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportModalPreview_W(Self: TfrReport; const T: Boolean);
begin Self.ModalPreview := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportModalPreview_R(Self: TfrReport; var T: Boolean);
begin T := Self.ModalPreview; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportInitialZoom_W(Self: TfrReport; const T: TfrPreviewZoom);
begin Self.InitialZoom := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportInitialZoom_R(Self: TfrReport; var T: TfrPreviewZoom);
begin T := Self.InitialZoom; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportGrayedButtons_W(Self: TfrReport; const T: Boolean);
begin Self.GrayedButtons := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportGrayedButtons_R(Self: TfrReport; var T: Boolean);
begin T := Self.GrayedButtons; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefaultCopies_W(Self: TfrReport; const T: Integer);
begin Self.DefaultCopies := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefaultCopies_R(Self: TfrReport; var T: Integer);
begin T := Self.DefaultCopies; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDataset_W(Self: TfrReport; const T: TfrDataset);
begin Self.Dataset := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDataset_R(Self: TfrReport; var T: TfrDataset);
begin T := Self.Dataset; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDetailReports_R(Self: TfrReport; var T: TlrDetailReports);
begin T := Self.DetailReports; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefaultCollate_W(Self: TfrReport; const T: boolean);
begin Self.DefaultCollate := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefaultCollate_R(Self: TfrReport; var T: boolean);
begin T := Self.DefaultCollate; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefExportFileName_W(Self: TfrReport; const T: string);
begin Self.DefExportFileName := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefExportFileName_R(Self: TfrReport; var T: string);
begin T := Self.DefExportFileName; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefExportFilterClass_W(Self: TfrReport; const T: string);
begin Self.DefExportFilterClass := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDefExportFilterClass_R(Self: TfrReport; var T: string);
begin T := Self.DefExportFilterClass; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportScript_W(Self: TfrReport; const T: TfrScriptStrings);
begin Self.Script := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportScript_R(Self: TfrReport; var T: TfrScriptStrings);
begin T := Self.Script; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportValues_W(Self: TfrReport; const T: TfrValues);
begin Self.Values := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportValues_R(Self: TfrReport; var T: TfrValues);
begin T := Self.Values; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportVariables_W(Self: TfrReport; const T: TStrings);
begin Self.Variables := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportVariables_R(Self: TfrReport; var T: TStrings);
begin T := Self.Variables; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportEMFPages_W(Self: TfrReport; const T: TfrEMFPages);
begin Self.EMFPages := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportEMFPages_R(Self: TfrReport; var T: TfrEMFPages);
begin T := Self.EMFPages; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPages_R(Self: TfrReport; var T: TfrPages);
begin T := Self.Pages; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportLastChange_W(Self: TfrReport; const T: TDateTime);
begin Self.ReportLastChange := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportLastChange_R(Self: TfrReport; var T: TDateTime);
begin T := Self.ReportLastChange; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportCreateDate_W(Self: TfrReport; const T: TDateTime);
begin Self.ReportCreateDate := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportCreateDate_R(Self: TfrReport; var T: TDateTime);
begin T := Self.ReportCreateDate; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionBuild_W(Self: TfrReport; const T: string);
begin Self.ReportVersionBuild := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionBuild_R(Self: TfrReport; var T: string);
begin T := Self.ReportVersionBuild; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionRelease_W(Self: TfrReport; const T: string);
begin Self.ReportVersionRelease := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionRelease_R(Self: TfrReport; var T: string);
begin T := Self.ReportVersionRelease; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionMinor_W(Self: TfrReport; const T: string);
begin Self.ReportVersionMinor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionMinor_R(Self: TfrReport; var T: string);
begin T := Self.ReportVersionMinor; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionMajor_W(Self: TfrReport; const T: string);
begin Self.ReportVersionMajor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportVersionMajor_R(Self: TfrReport; var T: string);
begin T := Self.ReportVersionMajor; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportAutor_W(Self: TfrReport; const T: string);
begin Self.ReportAutor := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportReportAutor_R(Self: TfrReport; var T: string);
begin T := Self.ReportAutor; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportComments_W(Self: TfrReport; const T: TStringList);
begin Self.Comments := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportComments_R(Self: TfrReport; var T: TStringList);
begin T := Self.Comments; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportKeyWords_W(Self: TfrReport; const T: string);
begin Self.KeyWords := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportKeyWords_R(Self: TfrReport; var T: string);
begin T := Self.KeyWords; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportSubject_W(Self: TfrReport; const T: string);
begin Self.Subject := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportSubject_R(Self: TfrReport; var T: string);
begin T := Self.Subject; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportExportFilename_W(Self: TfrReport; const T: string);
Begin Self.ExportFilename := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportExportFilename_R(Self: TfrReport; var T: string);
Begin T := Self.ExportFilename; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportFileName_W(Self: TfrReport; const T: String);
Begin Self.FileName := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportFileName_R(Self: TfrReport; var T: String);
Begin T := Self.FileName; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportFinalPass_W(Self: TfrReport; const T: Boolean);
Begin Self.FinalPass := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportFinalPass_R(Self: TfrReport; var T: Boolean);
Begin T := Self.FinalPass; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDoublePass_W(Self: TfrReport; const T: WordBool);
Begin Self.DoublePass := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportDoublePass_R(Self: TfrReport; var T: WordBool);
Begin T := Self.DoublePass; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPrintToDefault_W(Self: TfrReport; const T: WordBool);
Begin Self.PrintToDefault := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportPrintToDefault_R(Self: TfrReport; var T: WordBool);
Begin T := Self.PrintToDefault; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportTerminated_W(Self: TfrReport; const T: Boolean);
Begin Self.Terminated := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportTerminated_R(Self: TfrReport; var T: Boolean);
Begin T := Self.Terminated; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportCanRebuild_W(Self: TfrReport; const T: Boolean);
Begin Self.CanRebuild := T; end;

(*----------------------------------------------------------------------------*)
procedure TfrReportCanRebuild_R(Self: TfrReport; var T: Boolean);
Begin T := Self.CanRebuild; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TfrReport(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrReport) do
  begin
    RegisterMethod(@TfrReport.FormatValue, 'FormatValue');
    RegisterMethod(@TfrReport.FindVariable, 'FindVariable');
    RegisterMethod(@TfrReport.GetVariableValue, 'GetVariableValue');
    RegisterMethod(@TfrReport.GetVarList, 'GetVarList');
    RegisterMethod(@TfrReport.GetIntrpValue, 'GetIntrpValue');
    RegisterMethod(@TfrReport.GetCategoryList, 'GetCategoryList');
    RegisterMethod(@TfrReport.FindObject, 'FindObject');
    RegisterMethod(@TfrReport.FillQueryParams, 'FillQueryParams');
    RegisterMethod(@TfrReport.ChangePrinter, 'ChangePrinter');
    RegisterPropertyHelper(@TfrReportSubject_R,@TfrReportSubject_W,'Subject');
    RegisterPropertyHelper(@TfrReportKeyWords_R,@TfrReportKeyWords_W,'KeyWords');
    RegisterPropertyHelper(@TfrReportComments_R,@TfrReportComments_W,'Comments');
    RegisterPropertyHelper(@TfrReportReportAutor_R,@TfrReportReportAutor_W,'ReportAutor');
    RegisterPropertyHelper(@TfrReportReportVersionMajor_R,@TfrReportReportVersionMajor_W,'ReportVersionMajor');
    RegisterPropertyHelper(@TfrReportReportVersionMinor_R,@TfrReportReportVersionMinor_W,'ReportVersionMinor');
    RegisterPropertyHelper(@TfrReportReportVersionRelease_R,@TfrReportReportVersionRelease_W,'ReportVersionRelease');
    RegisterPropertyHelper(@TfrReportReportVersionBuild_R,@TfrReportReportVersionBuild_W,'ReportVersionBuild');
    RegisterPropertyHelper(@TfrReportReportCreateDate_R,@TfrReportReportCreateDate_W,'ReportCreateDate');
    RegisterPropertyHelper(@TfrReportReportLastChange_R,@TfrReportReportLastChange_W,'ReportLastChange');
    RegisterPropertyHelper(@TfrReportPages_R,nil,'Pages');
    RegisterPropertyHelper(@TfrReportVariables_R,@TfrReportVariables_W,'Variables');
    RegisterPropertyHelper(@TfrReportValues_R,@TfrReportValues_W,'Values');
    RegisterPropertyHelper(@TfrReportScript_R,@TfrReportScript_W,'Script');
    RegisterPropertyHelper(@TfrReportDefExportFilterClass_R,@TfrReportDefExportFilterClass_W,'DefExportFilterClass');
    RegisterPropertyHelper(@TfrReportDefExportFileName_R,@TfrReportDefExportFileName_W,'DefExportFileName');
    RegisterPropertyHelper(@TfrReportDefaultCollate_R,@TfrReportDefaultCollate_W,'DefaultCollate');
    RegisterPropertyHelper(@TfrReportDefaultCopies_R,@TfrReportDefaultCopies_W,'DefaultCopies');
    RegisterPropertyHelper(@TfrReportGrayedButtons_R,@TfrReportGrayedButtons_W,'GrayedButtons');
    RegisterPropertyHelper(@TfrReportInitialZoom_R,@TfrReportInitialZoom_W,'InitialZoom');
    RegisterPropertyHelper(@TfrReportModalPreview_R,@TfrReportModalPreview_W,'ModalPreview');
    RegisterPropertyHelper(@TfrReportModifyPrepared_R,@TfrReportModifyPrepared_W,'ModifyPrepared');
    RegisterPropertyHelper(@TfrReportOptions_R,@TfrReportOptions_W,'Options');
    RegisterPropertyHelper(@TfrReportPreviewButtons_R,@TfrReportPreviewButtons_W,'PreviewButtons');
    RegisterPropertyHelper(@TfrReportRebuildPrinter_R,@TfrReportRebuildPrinter_W,'RebuildPrinter');
    RegisterPropertyHelper(@TfrReportReportType_R,@TfrReportReportType_W,'ReportType');
    RegisterPropertyHelper(@TfrReportTitle_R,@TfrReportTitle_W,'Title');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_LR_Class(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TfrMemoStrings) do
  RIRegister_TfrObject(CL);
  RIRegister_TfrView(CL);
  RIRegister_TfrStretcheable(CL);
  RIRegister_TfrControl(CL);
  RIRegister_TfrNonVisualControl(CL);
  RIRegister_TfrCustomMemoView(CL);
  RIRegister_TfrMemoView(CL);
  RIRegister_TfrBandView(CL);
  RIRegister_TfrSubReportView(CL);
  RIRegister_TfrPictureView(CL);
  RIRegister_TfrLineView(CL);
  RIRegister_TfrRect(CL);
  RIRegister_TfrBand(CL);
  RIRegister_TfrValue(CL);
  RIRegister_TfrValues(CL);
  RIRegister_TfrPage(CL);
  RIRegister_TfrPageReport(CL);
  RIRegister_TfrPageDialog(CL);
  RIRegister_TfrPages(CL);
  RIRegister_TfrReport(CL);
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_LR_Class_Routines(S: TPSExec);
begin
 S.RegisterDelphiFunction(@frCreateObject, 'frCreateObject', cdRegister);
 S.RegisterDelphiFunction(@frRegisterObject, 'frRegisterObject', cdRegister);
 S.RegisterDelphiFunction(@frRegisterObject, 'frRegisterObject', cdRegister);
 S.RegisterDelphiFunction(@frSetAddinEditor, 'frSetAddinEditor', cdRegister);
 S.RegisterDelphiFunction(@frSetAddinIcon, 'frSetAddinIcon', cdRegister);
 S.RegisterDelphiFunction(@frSetAddinHint, 'frSetAddinHint', cdRegister);
 S.RegisterDelphiFunction(@frRegisterExportFilter, 'frRegisterExportFilter', cdRegister);
 S.RegisterDelphiFunction(@frRegisterFunctionLibrary, 'frRegisterFunctionLibrary', cdRegister);
 S.RegisterDelphiFunction(@frRegisterTool, 'frRegisterTool', cdRegister);
 S.RegisterDelphiFunction(@GetDefaultDataSet, 'GetDefaultDataSet', cdRegister);
 S.RegisterDelphiFunction(@SetBit, 'SetBit', cdRegister);
 S.RegisterDelphiFunction(@frGetBandName, 'frGetBandName', cdRegister);
 S.RegisterDelphiFunction(@frSelectHyphenDictionary, 'frSelectHyphenDictionary', cdRegister);
 S.RegisterDelphiFunction(@FindObjectProps, 'FindObjectProps', cdRegister);
 S.RegisterDelphiFunction(@ExportFilters, 'ExportFilters', cdRegister);
end;

(*----------------------------------------------------------------------------*)

procedure lr_psirunt(SE: TPSExec; CL: TPSRuntimeClassImporter);
begin
  RIRegister_TFPList(CL);
  RIRegister_TFont(CL);
  RIRegister_LR_Class(CL);
  RIRegister_LR_Class_Routines(se);
  RIRegister_TLRDataSetControl(CL);
  RIRegister_TlrUserDSControl(CL);
  RIRegister_TfrVariables(CL);
end;

end.


unit lr_psic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, uPSCompiler;

procedure lr_psicomp(CL: TPSPascalCompiler);

implementation

uses
  LR_Class, Graphics;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
Function BoolToStr(value : boolean) : string;
Begin If value then Result := 'TRUE' else Result := 'FALSE' End;

procedure SIRegister_TFPList(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TFPList') do
  begin
    RegisterMethod('Procedure AddList( AList : TFPList)');
    RegisterMethod('Function Add( Item : ___Pointer) : Integer');
    RegisterMethod('Procedure Clear');
    RegisterMethod('Procedure Delete( Index : Integer)');
    RegisterMethod('Procedure Error( const Msg : string; Data : PtrInt)');
    RegisterMethod('Procedure Exchange( Index1, Index2 : Integer)');
    RegisterMethod('Function Expand : TFPList');
    RegisterMethod('Function Extract( Item : ___Pointer) : ___Pointer');
    RegisterMethod('Function First : TObject');
    RegisterMethod('Function GetEnumerator : TFPListEnumerator');
    RegisterMethod('Function IndexOf( Item : ___Pointer) : Integer');
    RegisterMethod('Function IndexOfItem( Item : ___Pointer; Direction : TDirection) : Integer');
    RegisterMethod('Procedure Insert( Index : Integer; Item : Pointer)');
    RegisterMethod('Function Last : Pointer');
    RegisterMethod('Procedure Move( CurIndex, NewIndex : Integer)');
    RegisterMethod('Procedure Assign( ListA : TFPList; AOperator : TListAssignOp; ListB : TFPList)');
    RegisterMethod('Function Remove( Item : ___Pointer) : Integer');
    RegisterMethod('Procedure Pack');
    RegisterMethod('Procedure Sort( Compare : TListSortCompare)');
    RegisterMethod('Procedure ForEachCall( proc2call : TListCallback; arg : pointer)');
    RegisterMethod('Procedure ForEachCall( proc2call : TListStaticCallback; arg : pointer)');
    RegisterProperty('Capacity', 'Integer', iptrw);
    RegisterProperty('Count', 'Integer', iptrw);
    RegisterProperty('Items', '___Pointer Integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TFont(CL: TPSPascalCompiler);
begin
  with CL.AddClass(CL.FindClass('TPersistent'),TFont) do
  begin
    RegisterPublishedProperties;
    RegisterMethod('Function IsDefault : boolean');
    RegisterMethod('Function IsEqual( AFont : TFont) : boolean');
    RegisterProperty('IsMonoSpace', 'boolean', iptr);
    RegisterProperty('PixelsPerInch', 'Integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrVariables(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TfrVariables') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Procedure Clear');
    RegisterMethod('Procedure Delete( Index : Integer)');
    RegisterMethod('Function IndexOf( aName : String) : Integer');
    RegisterProperty('Variable', 'Variant String', iptrw);
    SetDefaultPropery('Variable');
    RegisterProperty('Value', 'Variant Integer', iptrw);
    RegisterProperty('Name', 'String Integer', iptr);
    RegisterProperty('Count', 'Integer', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrUserDSControl(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrNonVisualControl'),'TlrUserDSControl') do
  begin
    RegisterProperty('PSOnCheckEof', 'String', iptrw);
    RegisterProperty('PSOnFirst', 'String', iptrw);
    RegisterProperty('PSOnNext', 'String', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TLRDataSetControl(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrNonVisualControl'),'TLRDataSetControl') do
  begin
    RegisterMethod('Procedure Open');
    RegisterMethod('Procedure Close');
    RegisterMethod('Procedure First');
    RegisterMethod('Procedure Next');
    RegisterMethod('Procedure Last');
    RegisterMethod('Procedure Prior');
    RegisterProperty('DataSet', 'TDataSet', iptrw);
    RegisterProperty('lrDBDataSet', 'TfrDBDataSet', iptr);
    RegisterProperty('lrDataSource', 'TDataSource', iptr);
    RegisterProperty('FieldValue', 'Variant String', iptr);
    RegisterProperty('FieldText', 'String String', iptr);
    RegisterProperty('FieldName', 'String Integer', iptr);
    RegisterProperty('DisplayLabel', 'String String', iptr);
    RegisterProperty('DisplayWidth', 'Integer String', iptr);
    RegisterProperty('Active', 'boolean', iptrw);
    RegisterProperty('EOF', 'boolean', iptr);
    RegisterProperty('RecordCount', 'integer', iptr);
    RegisterProperty('FieldCount', 'integer', iptr);
    RegisterProperty('Filter', 'string', iptrw);
    RegisterProperty('DataSource', 'string', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrObject(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TfrObject') do
  begin
    RegisterProperty('x', 'Integer', iptrw);
    RegisterProperty('y', 'Integer', iptrw);
    RegisterProperty('dx', 'Integer', iptrw);
    RegisterProperty('dy', 'Integer', iptrw);
    RegisterMethod('Constructor Create( AOwnerPage : TfrPage)');
    RegisterMethod('Procedure BeginUpdate');
    RegisterMethod('Procedure EndUpdate');
    RegisterMethod('Procedure CreateUniqueName');
    RegisterProperty('Memo', 'TfrMemoStrings', iptrw);
    RegisterProperty('Script', 'TfrScriptStrings', iptrw);
    RegisterProperty('Left', 'Integer', iptrw);
    RegisterProperty('Top', 'Integer', iptrw);
    RegisterProperty('Width', 'Integer', iptrw);
    RegisterProperty('Height', 'Integer', iptrw);
    RegisterProperty('DesignOptions', 'TlrDesignOptions', iptr);
    RegisterProperty('PSOnEnter', 'String', iptrw);
    RegisterProperty('Name', 'string', iptrw);
    RegisterProperty('Visible', 'Boolean', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrObject'),'TfrView') do
  begin
    RegisterProperty('Parent', 'TfrBand', iptrw);
    RegisterProperty('ID', 'Integer', iptrw);
    RegisterProperty('Typ', 'Byte', iptrw);
    RegisterProperty('Selected', 'Boolean', iptrw);
    RegisterProperty('OriginalRect', 'TRect', iptrw);
    RegisterProperty('ScaleX', 'Double', iptrw);
    RegisterProperty('ScaleY', 'Double', iptrw);
    RegisterProperty('OffsX', 'Integer', iptrw);
    RegisterProperty('OffsY', 'Integer', iptrw);
    RegisterProperty('IsPrinting', 'Boolean', iptrw);
    RegisterProperty('Flags', 'Word', iptrw);
    RegisterProperty('DRect', 'TRect', iptrw);
    RegisterProperty('ParentBandType', 'TfrBandType', iptrw);
    RegisterMethod('Procedure SetBounds( aLeft, aTop, aWidth, aHeight : Integer)');
    RegisterProperty('FillColor', 'TColor', iptrw);
    RegisterProperty('Stretched', 'Boolean', iptrw);
    RegisterProperty('Frames', 'TfrFrameBorders', iptrw);
    RegisterProperty('FrameColor', 'TColor', iptrw);
    RegisterProperty('FrameStyle', 'TfrFrameStyle', iptrw);
    RegisterProperty('FrameWidth', 'Double', iptrw);
    RegisterProperty('Format', 'Integer', iptrw);
    RegisterProperty('FormatStr', 'String', iptrw);
    RegisterProperty('StreamMode', 'TfrStreamMode', iptrw);
    RegisterProperty('Restrictions', 'TlrRestrictions', iptrw);
    RegisterProperty('FindHighlight', 'boolean', iptrw);
    RegisterProperty('GapX', 'Integer', iptrw);
    RegisterProperty('GapY', 'Integer', iptrw);
    RegisterProperty('Left', 'double', iptrw);
    RegisterProperty('Top', 'double', iptrw);
    RegisterProperty('Tag', 'string', iptrw);
    RegisterProperty('URLInfo', 'string', iptrw);
    RegisterProperty('Width', 'double', iptrw);
    RegisterProperty('Height', 'double', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrStretcheable(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrStretcheable') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrControl(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrControl') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrNonVisualControl(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrControl'),'TfrNonVisualControl') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrCustomMemoView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrStretcheable'),'TfrCustomMemoView') do
  begin
    RegisterProperty('Adjust', 'Integer', iptrw);
    RegisterProperty('Highlight', 'TfrHighlightAttr', iptrw);
    RegisterProperty('HighlightStr', 'String', iptrw);
    RegisterProperty('CharacterSpacing', 'Integer', iptrw);
    RegisterProperty('LastLine', 'boolean', iptrw);
    RegisterProperty('FirstLine', 'boolean', iptrw);
    RegisterProperty('Justify', 'boolean', iptrw);
    RegisterProperty('Cursor', 'TCursor', iptrw);
    RegisterProperty('DetailReport', 'string', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('Layout', 'TTextLayout', iptrw);
    RegisterProperty('Angle', 'Byte', iptrw);
    RegisterProperty('WordBreak', 'Boolean', iptrw);
    RegisterProperty('WordWrap', 'Boolean', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('HideDuplicates', 'Boolean', iptrw);
    RegisterProperty('HideZeroValues', 'Boolean', iptrw);
    RegisterProperty('OnClick', 'TfrScriptStrings', iptrw);
    RegisterProperty('OnMouseEnter', 'TfrScriptStrings', iptrw);
    RegisterProperty('OnMouseLeave', 'TfrScriptStrings', iptrw);
    RegisterProperty('ParagraphGap', 'integer', iptrw);
    RegisterProperty('LineSpacing', 'integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrMemoView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrCustomMemoView'),'TfrMemoView') do
  begin

  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrBandView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrBandView') do
  begin
    RegisterProperty('DataSet', 'String', iptrw);
    RegisterProperty('GroupCondition', 'String', iptrw);
    RegisterProperty('Child', 'String', iptrw);
    RegisterProperty('BandType', 'TfrBandType', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrSubReportView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrSubReportView') do
  begin
    RegisterProperty('SubPage', 'TfrPage', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrPictureView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrPictureView') do
  begin
    RegisterProperty('Picture', 'TPicture', iptrw);
    RegisterProperty('KeepAspect', 'boolean', iptrw);
    RegisterProperty('Centered', 'boolean', iptrw);
    RegisterProperty('SharedName', 'string', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrLineView(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrView'),'TfrLineView') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrRect(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TPersistent'),'TfrRect') do
  begin
    RegisterProperty('AsRect', 'TRect', iptrw);
    RegisterProperty('Left', 'Integer', iptrw);
    RegisterProperty('Top', 'Integer', iptrw);
    RegisterProperty('Right', 'Integer', iptrw);
    RegisterProperty('Bottom', 'Integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrBand(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrObject'),'TfrBand') do
  begin
    RegisterProperty('EOFReached', 'Boolean', iptrw);
    RegisterProperty('MaxDY', 'Integer', iptrw);
    RegisterProperty('Typ', 'TfrBandType', iptrw);
    RegisterProperty('PrintIfSubsetEmpty', 'Boolean', iptrw);
    RegisterProperty('NewPageAfter', 'Boolean', iptrw);
    RegisterProperty('Stretched', 'Boolean', iptrw);
    RegisterProperty('PageBreak', 'Boolean', iptrw);
    RegisterProperty('PrintChildIfNotVisible', 'Boolean', iptrw);
    RegisterProperty('Objects', 'TFpList', iptrw);
    RegisterProperty('DataSet', 'TfrDataSet', iptrw);
    RegisterProperty('IsVirtualDS', 'Boolean', iptrw);
    RegisterProperty('VCDataSet', 'TfrDataSet', iptrw);
    RegisterProperty('IsVirtualVCDS', 'Boolean', iptrw);
    RegisterProperty('GroupCondition', 'String', iptrw);
    RegisterProperty('ForceNewPage', 'Boolean', iptrw);
    RegisterProperty('ForceNewColumn', 'Boolean', iptrw);
    RegisterMethod('Constructor Create2( ATyp : TfrBandType; AParent : TfrPage);');
    RegisterMethod('Function IsDataBand : boolean');
    RegisterProperty('Name', 'string', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrValue(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TOBJECT', 'TfrValue') do
  with CL.AddClassN(CL.FindClass('TOBJECT'),'TfrValue') do
  begin
    RegisterProperty('Typ', 'TfrValueType', iptrw);
    RegisterProperty('OtherKind', 'Integer', iptrw);
    RegisterProperty('DataSet', 'String', iptrw);
    RegisterProperty('Field', 'String', iptrw);
    RegisterProperty('DSet', 'TfrTDataSet', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrValues(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TPersistent', 'TfrValues') do
  with CL.AddClassN(CL.FindClass('TPersistent'),'TfrValues') do
  begin
    RegisterMethod('Constructor Create');
    RegisterMethod('Function AddValue : Integer');
    RegisterMethod('Function FindVariable( const s : String) : TfrValue');
    RegisterMethod('Procedure Clear');
    RegisterProperty('Items', 'TStringList', iptrw);
    RegisterProperty('Objects', 'TfrValue Integer', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrPage(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TfrObject', 'TfrPage') do
  with CL.AddClassN(CL.FindClass('TfrObject'),'TfrPage') do
  begin
    RegisterMethod('Constructor Create( ASize, AWidth, AHeight : Integer; AOr : TPrinterOrientation)');
    RegisterMethod('Constructor CreatePage');
    RegisterMethod('Function TopMargin : Integer');
    RegisterMethod('Function BottomMargin : Integer');
    RegisterMethod('Function LeftMargin : Integer');
    RegisterMethod('Function RightMargin : Integer');
    RegisterMethod('function Add(AObject: TfrObject): Integer');
    RegisterMethod('Procedure Clear');
    RegisterMethod('Procedure Delete( Index : Integer)');
    RegisterMethod('Function FindObjectByID( ID : Integer) : Integer');
    RegisterMethod('Function FindObject( aName : String) : TfrObject');
    RegisterMethod('Function FindRTObject( const aName : String) : TfrObject');
    RegisterMethod('Procedure ChangePaper( ASize, AWidth, AHeight : Integer; AOr : TPrinterOrientation)');
    RegisterProperty('pgSize', 'Integer', iptrw);
    RegisterProperty('PrnInfo', 'TfrPrnInfo', iptrw);
    RegisterProperty('Objects', 'TFpList', iptrw);
    RegisterProperty('RTObjects', 'TFpList', iptrw);
    RegisterProperty('CurY', 'Integer', iptrw);
    RegisterProperty('CurBottomY', 'Integer', iptrw);
    RegisterProperty('ColCount', 'Integer', iptrw);
    RegisterProperty('ColWidth', 'Integer', iptrw);
    RegisterProperty('ColGap', 'Integer', iptrw);
    RegisterProperty('UseMargins', 'Boolean', iptrw);
    RegisterProperty('Margins', 'TfrRect', iptrw);
    RegisterProperty('PrintToPrevPage', 'Boolean', iptrw);
    RegisterProperty('Orientation', 'TPrinterOrientation', iptrw);
    RegisterProperty('LayoutOrder', 'TLayoutOrder', iptrw);
    RegisterProperty('LastRowHeight', 'Integer', iptrw);
    RegisterProperty('RowStarted', 'boolean', iptrw);
    RegisterProperty('LastBandType', 'TfrBandType', iptrw);
    RegisterProperty('PageIndex', 'integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrPageReport(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrPage'),'TfrPageReport') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrPageDialog(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TfrPage'),'TfrPageDialog') do
  begin
    RegisterMethod('function ShowModal : Integer');
    RegisterProperty('Form', 'TfrDialogForm', iptr);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('ModalResult', 'Integer', iptRW);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrPages(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TObject'),'TfrPages') do
  begin
    RegisterMethod('Constructor Create( AParent : TfrReport)');
    RegisterMethod('Procedure Clear');
    RegisterMethod('Function Add( const aClassName : string) : TfrPage');
    RegisterMethod('Procedure Delete( Index : Integer)');
    RegisterMethod('Procedure Move( OldIndex, NewIndex : Integer)');
    RegisterMethod('Function PageByName( APageName : string) : TfrPage');
    RegisterProperty('Pages', 'TfrPage Integer', iptr);
    SetDefaultPropery('Pages');
    RegisterProperty('Count', 'Integer', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TfrReport(CL: TPSPascalCompiler);
begin
  with CL.AddClassN(CL.FindClass('TComponent'),'TfrReport') do
  begin
    RegisterMethod('Function FormatValue( V : Variant; AFormat : Integer; const AFormatStr : String) : String');
    RegisterMethod('Function FindVariable( Variable : String) : Integer');
    RegisterMethod('Procedure GetVariableValue( const s : String; var aValue : Variant)');
    RegisterMethod('Procedure GetVarList( CatNo : Integer; List : TStrings)');
    RegisterMethod('Procedure GetIntrpValue( AName : String; var AValue : Variant)');
    RegisterMethod('Procedure GetCategoryList( List : TStrings)');
    RegisterMethod('Function FindObject( const aName : String) : TfrObject');
    RegisterMethod('Procedure FillQueryParams');
    RegisterMethod('Function ChangePrinter( OldIndex, NewIndex : Integer) : Boolean');
    RegisterProperty('Subject', 'string', iptrw);
    RegisterProperty('KeyWords', 'string', iptrw);
    RegisterProperty('Comments', 'TStringList', iptrw);
    RegisterProperty('ReportAutor', 'string', iptrw);
    RegisterProperty('ReportVersionMajor', 'string', iptrw);
    RegisterProperty('ReportVersionMinor', 'string', iptrw);
    RegisterProperty('ReportVersionRelease', 'string', iptrw);
    RegisterProperty('ReportVersionBuild', 'string', iptrw);
    RegisterProperty('ReportCreateDate', 'TDateTime', iptrw);
    RegisterProperty('ReportLastChange', 'TDateTime', iptrw);
    RegisterProperty('Pages', 'TfrPages', iptr);
    RegisterProperty('Variables', 'TStrings', iptrw);
    RegisterProperty('Values', 'TfrValues', iptrw);
    RegisterProperty('Script', 'TfrScriptStrings', iptrw);
    RegisterProperty('DefExportFilterClass', 'string', iptrw);
    RegisterProperty('DefExportFileName', 'string', iptrw);
    RegisterProperty('DefaultCollate', 'boolean', iptrw);
    RegisterProperty('DefaultCopies', 'Integer', iptrw);
    RegisterProperty('GrayedButtons', 'Boolean', iptrw);
    RegisterProperty('InitialZoom', 'TfrPreviewZoom', iptrw);
    RegisterProperty('ModalPreview', 'Boolean', iptrw);
    RegisterProperty('ModifyPrepared', 'Boolean', iptrw);
    RegisterProperty('Options', 'TfrReportOptions', iptrw);
    RegisterProperty('PreviewButtons', 'TfrPreviewButtons', iptrw);
    RegisterProperty('RebuildPrinter', 'boolean', iptrw);
    RegisterProperty('ReportType', 'TfrReportType', iptrw);
    RegisterProperty('Title', 'String', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_Graphics(CL: TPSPascalCompiler);
begin
  CL.AddTypeS('TGraphicsColor', 'Integer');
  CL.AddTypeS('TColor', 'TGraphicsColor');
  CL.AddTypeS('TFontPitch', '( fpDefault, fpVariable, fpFixed )');
  CL.AddTypeS('TFontStyle', '( fsBold, fsItalic, fsStrikeOut, fsUnderline )');
  CL.AddTypeS('TFontStyles', 'set of TFontStyle');
  CL.AddTypeS('TFontCharSet', 'Integer');
  CL.AddTypeS('TFontQuality', '( fqDefault, fqDraft, fqProof, fqNonAntialiased,'
   +' fqAntialiased, fqCleartype, fqCleartypeNatural )');
  SIRegister_TFont(CL);
  CL.AddConstantN('clBlack','Integer').SetInt(clBlack);
  CL.AddConstantN('clMaroon','Integer').SetInt(clMaroon);
  CL.AddConstantN('clGreen','Integer').SetInt(clGreen);
  CL.AddConstantN('clOlive','Integer').SetInt(clOlive);
  CL.AddConstantN('clNavy','Integer').SetInt(clNavy);
  CL.AddConstantN('clPurple','Integer').SetInt(clPurple);
  CL.AddConstantN('clTeal','Integer').SetInt(clTeal);
  CL.AddConstantN('clGray','Integer').SetInt(clGray);
  CL.AddConstantN('clSilver','Integer').SetInt(clSilver);
  CL.AddConstantN('clRed','Integer').SetInt(clRed);
  CL.AddConstantN('clLime','Integer').SetInt(clLime);
  CL.AddConstantN('clYellow','Integer').SetInt(clYellow);
  CL.AddConstantN('clBlue','Integer').SetInt(clBlue);
  CL.AddConstantN('clFuchsia','Integer').SetInt(clFuchsia);
  CL.AddConstantN('clAqua','Integer').SetInt(clAqua);
  CL.AddConstantN('clLtGray','Integer').SetInt(clLtGray);
  CL.AddConstantN('clDkGray','Integer').SetInt(clDkGray);
  CL.AddConstantN('clWhite','Integer').SetInt(clWhite);
  CL.AddConstantN('StandardColorsCount','LongInt').SetInt(StandardColorsCount);
  CL.AddConstantN('clMoneyGreen','Integer').SetInt(clMoneyGreen);
  CL.AddConstantN('clSkyBlue','Integer').SetInt(clSkyBlue);
  CL.AddConstantN('clCream','Integer').SetInt(clCream);
  CL.AddConstantN('clMedGray','Integer').SetInt(clMedGray);
  CL.AddConstantN('ExtendedColorCount','LongInt').SetInt(ExtendedColorCount);
  CL.AddConstantN('clNone','Integer').SetInt(clNone);
  CL.AddConstantN('clDefault','Integer').SetInt(clDefault);

  CL.AddConstantN('clScrollBar','Integer').SetInt(               clScrollBar              );
  CL.AddConstantN('clBackground','Integer').SetInt(              clBackground             );
  CL.AddConstantN('clActiveCaption','Integer').SetInt(           clActiveCaption          );
  CL.AddConstantN('clInactiveCaption','Integer').SetInt(         clInactiveCaption        );
  CL.AddConstantN('clMenu','Integer').SetInt(                    clMenu                   );
  CL.AddConstantN('clWindow','Integer').SetInt(                  clWindow                 );
  CL.AddConstantN('clWindowFrame','Integer').SetInt(             clWindowFrame            );
  CL.AddConstantN('clMenuText','Integer').SetInt(                clMenuText               );
  CL.AddConstantN('clWindowText','Integer').SetInt(              clWindowText             );
  CL.AddConstantN('clCaptionText','Integer').SetInt(             clCaptionText            );
  CL.AddConstantN('clActiveBorder','Integer').SetInt(            clActiveBorder           );
  CL.AddConstantN('clInactiveBorder','Integer').SetInt(          clInactiveBorder         );
  CL.AddConstantN('clAppWorkspace','Integer').SetInt(            clAppWorkspace           );
  CL.AddConstantN('clHighlight','Integer').SetInt(               clHighlight              );
  CL.AddConstantN('clHighlightText','Integer').SetInt(           clHighlightText          );
  CL.AddConstantN('clBtnFace','Integer').SetInt(                 clBtnFace                );
  CL.AddConstantN('clBtnShadow','Integer').SetInt(               clBtnShadow              );
  CL.AddConstantN('clGrayText','Integer').SetInt(                clGrayText               );
  CL.AddConstantN('clBtnText','Integer').SetInt(                 clBtnText                );
  CL.AddConstantN('clInactiveCaptionText','Integer').SetInt(     clInactiveCaptionText    );
  CL.AddConstantN('clBtnHighlight','Integer').SetInt(            clBtnHighlight           );
  CL.AddConstantN('cl3DDkShadow','Integer').SetInt(              cl3DDkShadow             );
  CL.AddConstantN('cl3DLight','Integer').SetInt(                 cl3DLight                );
  CL.AddConstantN('clInfoText','Integer').SetInt(                clInfoText               );
  CL.AddConstantN('clInfoBk','Integer').SetInt(                  clInfoBk                 );

  CL.AddConstantN('clHotLight','Integer').SetInt(                clHotLight               );
  CL.AddConstantN('clGradientActiveCaption','Integer').SetInt(   clGradientActiveCaption  );
  CL.AddConstantN('clGradientInactiveCaption','Integer').SetInt( clGradientInactiveCaption);
  CL.AddConstantN('clMenuHighlight','Integer').SetInt(           clMenuHighlight          );
  CL.AddConstantN('clMenuBar','Integer').SetInt(                 clMenuBar                );
  CL.AddConstantN('clForm','Integer').SetInt(                    clForm                   );

  CL.AddConstantN('clColorDesktop','Integer').SetInt(            clColorDesktop           );
  CL.AddConstantN('cl3DFace','Integer').SetInt(                  cl3DFace                 );
  CL.AddConstantN('cl3DShadow','Integer').SetInt(                cl3DShadow               );
  CL.AddConstantN('cl3DHiLight','Integer').SetInt(               cl3DHiLight              );
  CL.AddConstantN('clBtnHiLight','Integer').SetInt(              clBtnHiLight             );

  CL.AddConstantN('clFirstSpecialColor','Integer').SetInt(clFirstSpecialColor);
  CL.AddConstantN('clMask','Integer').SetInt(clMask);
  CL.AddConstantN('clDontMask','Integer').SetInt(clDontMask);
  SIRegister_TFont(CL);
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_LR_Class(CL: TPSPascalCompiler);
begin
  CL.AddTypeS('TColor', 'Integer');
  CL.AddConstantN('lrMaxBandsInReport','LongInt').SetInt( 256);
  CL.AddConstantN('flStretched','LongWord').SetUInt( $01);
  CL.AddConstantN('flWordWrap','LongWord').SetUInt( $02);
  CL.AddConstantN('flWordBreak','LongWord').SetUInt( $04);
  CL.AddConstantN('flAutoSize','LongWord').SetUInt( $08);
  CL.AddConstantN('flHideDuplicates','LongWord').SetUInt( $10);
  CL.AddConstantN('flStartRecord','LongWord').SetUInt( $20);
  CL.AddConstantN('flEndRecord','LongWord').SetUInt( $40);
  CL.AddConstantN('flHideZeros','LongWord').SetUInt( $80);
  CL.AddConstantN('flBandNewPageAfter','LongInt').SetInt( 2);
  CL.AddConstantN('flBandPrintifSubsetEmpty','LongInt').SetInt( 4);
  CL.AddConstantN('flBandPageBreak','LongInt').SetInt( 8);
  CL.AddConstantN('flBandOnFirstPage','LongWord').SetUInt( $10);
  CL.AddConstantN('flBandOnLastPage','LongWord').SetUInt( $20);
  CL.AddConstantN('flBandRepeatHeader','LongWord').SetUInt( $40);
  CL.AddConstantN('flBandPrintChildIfNotVisible','LongWord').SetUInt( $80);
  CL.AddConstantN('flPictCenter','LongInt').SetInt( 2);
  CL.AddConstantN('flPictRatio','LongInt').SetInt( 4);
  CL.AddConstantN('flWantHook','LongWord').SetUInt( $8000);
  CL.AddConstantN('flIsDuplicate','LongWord').SetUInt( $4000);
  CL.AddConstantN('gtMemo','LongInt').SetInt( 0);
  CL.AddConstantN('gtPicture','LongInt').SetInt( 1);
  CL.AddConstantN('gtBand','LongInt').SetInt( 2);
  CL.AddConstantN('gtSubReport','LongInt').SetInt( 3);
  CL.AddConstantN('gtLine','LongInt').SetInt( 4);
  CL.AddConstantN('gtAddIn','LongInt').SetInt( 10);
  CL.AddConstantN('fmtText','LongInt').SetInt( 0);
  CL.AddConstantN('fmtNumber','LongInt').SetInt( 1);
  CL.AddConstantN('fmtDate','LongInt').SetInt( 2);
  CL.AddConstantN('fmtTime','LongInt').SetInt( 3);
  CL.AddConstantN('fmtBoolean','LongInt').SetInt( 4);
  CL.AddTypeS('TfrDrawMode', '( drAll, drCalcHeight, drAfterCalcHeight, drPart '
  +')');
  CL.AddTypeS('TfrBandType', '( btReportTitle, btReportSummary, btPageHeader, b'
  +'tPageFooter, btMasterHeader, btMasterData, btMasterFooter, btDetailHeader,'
  +' btDetailData, btDetailFooter, btSubDetailHeader, btSubDetailData, btSubDe'
  +'tailFooter, btOverlay, btColumnHeader, btColumnFooter, btGroupHeader, btGr'
  +'oupFooter, btCrossHeader, btCrossData, btCrossFooter, btChild, btNone )');
  CL.AddTypeS('TfrBandTypes', 'set of TfrBandType');
  CL.AddTypeS('TfrDataSetPosition', '( psLocal, psGlobal )');
  CL.AddTypeS('TfrValueType', '( vtNotAssigned, vtDBField, vtOther, vtFRVar )');
  CL.AddTypeS('TfrPageMode', '( pmNormal, pmBuildList )');
  CL.AddTypeS('TfrBandRecType', '( rtShowBand, rtFirst, rtNext )');
  CL.AddTypeS('TfrRgnType', '( rtNormal, rtExtended )');
  CL.AddTypeS('TfrReportType', '( rtSimple, rtMultiple )');
  CL.AddTypeS('TfrStreamMode', '( smDesigning, smPrinting )');
  CL.AddTypeS('TfrFrameBorder', '( frbLeft, frbTop, frbRight, frbBottom )');
  CL.AddTypeS('TfrFrameBorders', 'set of TfrFrameBorder');
  CL.AddTypeS('TfrFrameStyle', '( frsSolid, frsDash, frsDot, frsDashDot, frsDas'
  +'hDotDot, frsDouble )');
  CL.AddTypeS('TfrPageType', '( ptReport, ptDialog )');
  CL.AddTypeS('TfrReportOption', '( roIgnoreFieldNotFound, roIgnoreSymbolNotFou'
  +'nd, roHideDefaultFilter )');
  CL.AddTypeS('TfrReportOptions', 'set of TfrReportOption');
  CL.AddTypeS('TfrObjectType', '( otlReportView, otlUIControl )');
  CL.AddTypeS('TlrDesignOption', '( doUndoDisable, doChildComponent )');
  CL.AddTypeS('TlrDesignOptions', 'set of TlrDesignOption');
  CL.AddTypeS('TlrRestriction', '( lrrDontModify, lrrDontSize, lrrDontMove, lrr'
  +'DontDelete )');
  CL.AddTypeS('TlrRestrictions', 'set of TlrRestriction');
  CL.AddTypeS('TLayoutOrder', '( loColumns, loRows )');
  CL.AddClassN(CL.FindClass('TSTRINGLIST'),'TfrMemoStrings');
  SIRegister_TfrObject(CL);
  SIRegister_TfrView(CL);
  SIRegister_TfrStretcheable(CL);
  SIRegister_TfrControl(CL);
  SIRegister_TfrNonVisualControl(CL);
  SIRegister_TfrCustomMemoView(CL);
  SIRegister_TfrMemoView(CL);
  SIRegister_TfrBandView(CL);
  SIRegister_TfrSubReportView(CL);
  SIRegister_TfrPictureView(CL);
  SIRegister_TfrLineView(CL);
  SIRegister_TfrRect(CL);
  SIRegister_TfrBand(CL);
  SIRegister_TfrValue(CL);
  SIRegister_TfrValues(CL);
  SIRegister_TfrPage(CL);
  SIRegister_TfrPageReport(CL);
  SIRegister_TfrPageDialog(CL);
  SIRegister_TfrPages(CL);
  CL.AddTypeS('TfrDataType', '( dtDataSet, dtDataSource )');
  SIRegister_TfrReport(CL);
  CL.AddDelphiFunction('Function frCreateObject( Typ : Byte; const ClassName : String; AOwnerPage : TfrPage) : TfrView');
  SIRegister_TfrVariables(CL);
end;

procedure lr_psicomp(CL: TPSPascalCompiler);
begin
  SIRegister_TFPList(CL);
  SIRegister_Graphics(CL);
  SIRegister_LR_Class(CL);
  SIRegister_TLRDataSetControl(CL);
  SIRegister_TlrUserDSControl(CL);
end;

end.


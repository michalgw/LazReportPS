{ LazReport dialogs control

  Copyright (C) 2012-2014 alexs alexs75.at.yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit LRDialogControls;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, DB, LR_Class, Controls, StdCtrls, CheckLst,
  LMessages, LCLType, LCLIntf, Buttons, EditBtn, Themes, ButtonPanel, ExtCtrls,
  LR_DSet;

type
  TLRDialogControls = class(TComponent)
  end;

  { TlrVisualControl }

  TlrVisualControl = class(TfrControl)
  private
    FPSOnClick: String;
    function GetAutoSize: Boolean;
    function GetCaption: string;
    function GetColor: TColor;
    function GetEnabled: boolean;
    function GetFont: TFont;
    function GetHint: string;
    function GetOnClick: TfrScriptStrings;
    procedure SetAutoSize(AValue: Boolean);
    procedure SetCaption(AValue: string);
    procedure SetColor(AValue: TColor);
    procedure SetEnabled(AValue: boolean);
    procedure SetFont(AValue: TFont);
    procedure OnClickHandle(Sender: TObject);
    procedure SetHint(AValue: string);
    procedure SetOnClick(AValue: TfrScriptStrings);
  protected
    FControl: TControl;
    procedure SetVisible(AValue: Boolean);override;
    procedure PaintDesignControl; override;
    procedure SetName(const AValue: string); override;
    procedure AfterCreate;override;
    function CreateControl:TControl;virtual;abstract;
    procedure PSExecOnClick(ProcName: String; VControl: TlrVisualControl);
  public
    constructor Create(AOwnerPage:TfrPage); override;
    destructor Destroy; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure UpdateControlPosition; override;
    procedure AttachToParent; override;
    procedure Assign(Source: TPersistent); override;

    property Control: TControl read FControl write FControl;
    property AutoSize: Boolean read GetAutoSize write SetAutoSize;
    property Color:TColor read GetColor write SetColor;
    property Caption:string read GetCaption write SetCaption;
    property Text:string read GetCaption write SetCaption;
    property Font:TFont read GetFont write SetFont;
    property Hint:string read GetHint write SetHint;
    property OnClick:TfrScriptStrings read GetOnClick write SetOnClick;
    property PSOnClick: String read FPSOnClick write FPSOnClick;
  published
    property Enabled:boolean read GetEnabled write SetEnabled;
  end;
  TlrVisualControlClass = class of TlrVisualControl;

  { TlrLabel }

  TlrLabel = class(TlrVisualControl)
  private
    function GetAlignment: TAlignment;
    function GetWordWrap: boolean;
    procedure SetAlignment(AValue: TAlignment);
    procedure SetWordWrap(AValue: boolean);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Alignment: TAlignment read GetAlignment write SetAlignment;
    property WordWrap:boolean read GetWordWrap write SetWordWrap;
    property AutoSize;
    property Color;
    property Caption;
    property Font;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrEdit }

  TlrEdit = class(TlrVisualControl)
  private
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Text;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrMemo }

  TlrMemo = class(TlrVisualControl)
  private
    procedure MemoChange(Sender: TObject);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    function GetText:string;override;
    procedure SetText(AValue:string);override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
  published
    property Color;
    property Enabled;
    property Memo;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrButton }

  TlrButton = class(TlrVisualControl)
  private
    function GetKind: TBitBtnKind;
    procedure SetKind(AValue: TBitBtnKind);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize;
    property Color;
    property Enabled;
    property Caption;
    property Hint;
    property Kind: TBitBtnKind read GetKind write SetKind;
    property OnClick;
    property PSOnClick;
  end;

  { TlrCheckBox }

  TlrCheckBox = class(TlrVisualControl)
  private
    function GetChecked: boolean;
    procedure SetChecked(AValue: boolean);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    function GetCheckStyle(ACheck:boolean):TThemedButton;virtual;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property AutoSize;
    property Color;
    property Enabled;
    property Caption;
    property Hint;
    property Checked:boolean read GetChecked write SetChecked;
    property OnClick;
    property PSOnClick;
  end;

  { TlrRadioButton }

  TlrRadioButton = class(TlrCheckBox)
  protected
    function CreateControl:TControl;override;
    function GetCheckStyle(ACheck:boolean):TThemedButton;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
  end;

  { TlrListBox }

  TlrListBox = class(TlrVisualControl)
  private
    function GetItemIndex: integer;
    function GetItems: TStrings;
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color;
    property Enabled;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrComboBox }

  TlrComboBox = class(TlrVisualControl)
  private
    function GetDropDownCount: integer;
    function GetItemIndex: integer;
    function GetItems: TStrings;
    function GetStyle: TComboBoxStyle;
    procedure SetDropDownCount(AValue: integer);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
    procedure SetStyle(AValue: TComboBoxStyle);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color;
    property Enabled;
    property Text;
    property AutoSize;
    property Hint;
    property Style:TComboBoxStyle read GetStyle write SetStyle;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property DropDownCount:integer read GetDropDownCount write SetDropDownCount;
    property OnClick;
    property PSOnClick;
  end;

  { TlrDateEdit }

  TlrDateEdit = class(TlrVisualControl)
  private
    function GetDate: TDateTime;
    procedure SetDate(AValue: TDateTime);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color;
    property Enabled;
    property Hint;
    property Date:TDateTime read GetDate write SetDate;
    property OnClick;
    property PSOnClick;
  end;

  { TlrButtonPanel }

  TlrButtonPanel = class(TlrVisualControl)
  private
    function GetButtonOrder: TButtonOrder;
    function GetShowButtons: TPanelButtons;
    procedure SetButtonOrder(AValue: TButtonOrder);
    procedure SetShowButtons(AValue: TPanelButtons);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    procedure AfterCreate;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure UpdateControlPosition; override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ButtonOrder: TButtonOrder read GetButtonOrder write SetButtonOrder default boDefault;
    property ShowButtons: TPanelButtons read GetShowButtons write SetShowButtons default DefShowButtons;
    property Color;
    property Enabled;
    property Text;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrCheckListBox }

  TlrCheckListBox = class(TlrVisualControl)
  private
    function GetChecked(AIndex: Integer): Boolean;
    function GetItemIndex: integer;
    function GetItems: TStrings;
    function GetItemsCount: integer;
    procedure SetChecked(AIndex: Integer; AValue: Boolean);
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
    function ExecMetod(const AName: String; p1, p2, p3: Variant; var Val: Variant):boolean;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
    property Checked[AIndex: Integer]: Boolean read GetChecked write SetChecked;
  published
    property Color;
    property Enabled;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property ItemsCount:integer read GetItemsCount;
    property Hint;
    property OnClick;
    property PSOnClick;
  end;

  { TlrRadioGroup }

  TlrRadioGroup = class(TlrVisualControl)
  private
    function GetItemIndex: integer;
    function GetItems: TStrings;
    function GetItemsCount: integer;
    procedure SetItemIndex(AValue: integer);
    procedure SetItems(AValue: TStrings);
  protected
    procedure PaintDesignControl; override;
    function CreateControl:TControl;override;
  public
    constructor Create(AOwnerPage:TfrPage); override;
    procedure LoadFromXML(XML: TLrXMLConfig; const Path: String); override;
    procedure SaveToXML(XML: TLrXMLConfig; const Path: String); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color;
    property Enabled;
    property Items:TStrings read GetItems write SetItems;
    property ItemIndex:integer read GetItemIndex write SetItemIndex;
    property ItemsCount:integer read GetItemsCount;
    property Hint;
    property OnClick;
    property PSOnClick;
    property Caption;
  end;

procedure Register;

procedure DoRegsiterControl(var cmpBMP:TBitmap; lrClass:TlrVisualControlClass;
  PSCompImportProc: TfrPSCompImport = nil; PSExecImportProc: TfrPSExecImport = nil);
implementation

uses typinfo, types, lrDBDialogControls, math, lrFormStorage, uPSCompiler,
  uPSRuntime, uPSUtils;

{$R lrdialogcontrols_img.res}

procedure Register;
begin
  RegisterComponents('LazReport',[TLRDialogControls]);
end;

var
  lrBMP_LRLabel:TBitmap = nil;
  lrBMP_LREdit:TBitmap = nil;
  lrBMP_LRButton:TBitmap = nil;
  lrBMP_LRCheckBox:TBitmap = nil;
  lrBMP_LRComboBox:TBitmap = nil;
  lrBMP_LRRadioButton:TBitmap = nil;
  lrBMP_LRMemo:TBitmap = nil;
  lrBMP_LRListBox:TBitmap = nil;
  lrBMP_LRDateEdit:TBitmap = nil;
  lrBMP_LRButtonPanel:TBitmap = nil;
  lrBMP_LRCheckListBox:TBitmap = nil;
  lrBMP_LRRadioGroupBox:TBitmap = nil;

{ TlrRadioGroup }

function TlrRadioGroup.GetItemIndex: integer;
begin
  Result:=TRadioGroup(FControl).ItemIndex;
end;

function TlrRadioGroup.GetItems: TStrings;
begin
  Result:=TRadioGroup(FControl).Items;
end;

function TlrRadioGroup.GetItemsCount: integer;
begin
  Result:=TRadioGroup(FControl).Items.Count;
end;

procedure TlrRadioGroup.SetItemIndex(AValue: integer);
begin
  TRadioGroup(FControl).ItemIndex:=AValue;
end;

procedure TlrRadioGroup.SetItems(AValue: TStrings);
begin
  TRadioGroup(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrRadioGroup.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;

var
  details, details_chek: TThemedElementDetails;
  PaintRect: TRect;
  CSize: TSize;
begin
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := Color; // FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;

  AY:=DRect.Top + 3;
  aH:=Canvas.TextHeight('Wg');

  Canvas.TextRect(DRect, DRect.Left + 3, AY, Text);
  inc(AY, aH + 3);

  i:=0;

  Details := ThemeServices.GetElementDetails(tbRadioButtonUncheckedNormal);
  details_chek:=ThemeServices.GetElementDetails(tbRadioButtonCheckedNormal);
  CSize := ThemeServices.GetDetailSize(Details);


  while (AY < DRect.Bottom) and (i<TRadioGroup(FControl).Items.Count) do
  begin
    PaintRect := Bounds(DRect.Left, AY, CSize.cx, CSize.cy);
    if ItemIndex = i then
      ThemeServices.DrawElement(Canvas.Handle, details_chek, PaintRect, nil)
    else
      ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect, nil);



    S:=TRadioGroup(FControl).Items[i];
    Canvas.TextRect(DRect, DRect.Left + 3 + CSize.cx, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;


function TlrRadioGroup.CreateControl: TControl;
begin
  Result:=TRadioGroup.Create(nil);
  TRadioGroup(Result).OnClick:=@OnClickHandle;
end;

constructor TlrRadioGroup.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrRadioGroup';
  AutoSize:=false;
end;

procedure TlrRadioGroup.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
end;

procedure TlrRadioGroup.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
end;

procedure TlrRadioGroup.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrRadioGroup then
  begin
    Items.Assign(TlrRadioGroup(Source).Items);
    ItemIndex:=TlrRadioGroup(Source).ItemIndex;
  end;
end;

{ TlrCheckListBox }

function TlrCheckListBox.GetChecked(AIndex: Integer): Boolean;
begin
  Result := TCheckListBox(FControl).Checked[AIndex];
end;

function TlrCheckListBox.GetItemIndex: integer;
begin
  Result:=TCheckListBox(FControl).ItemIndex;
end;

function TlrCheckListBox.GetItems: TStrings;
begin
  Result:=TCheckListBox(FControl).Items;
end;

function TlrCheckListBox.GetItemsCount: integer;
begin
  Result:=TCheckListBox(FControl).Items.Count;
end;

procedure TlrCheckListBox.SetChecked(AIndex: Integer; AValue: Boolean);
begin
  TCheckListBox(FControl).Checked[AIndex] := AValue;
end;

procedure TlrCheckListBox.SetItemIndex(AValue: integer);
begin
  TCheckListBox(FControl).ItemIndex:=AValue;
end;

procedure TlrCheckListBox.SetItems(AValue: TStrings);
begin
  TCheckListBox(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrCheckListBox.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;

var
//  ChkBitmap: TBitmap;
//  XPos,YPos: Integer;
  details: TThemedElementDetails;
  PaintRect: TRect;
  CSize: TSize;
//  bmpAlign: TAlignment;
begin
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  AY:=DRect.Top + 1;
  i:=0;

  Details := ThemeServices.GetElementDetails(tbCheckBoxUncheckedNormal);
  CSize := ThemeServices.GetDetailSize(Details);

  aH:=Max(Canvas.TextHeight(Text) div 2, CSize.cy);

  while (AY < DRect.Bottom) and (i<TListBox(FControl).Items.Count) do
  begin
    PaintRect := Bounds(DRect.Left, AY, CSize.cx, CSize.cy);
    ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect, nil);


    S:=TCheckListBox(FControl).Items[i];
    Canvas.TextRect(DRect, DRect.Left + 3 + CSize.cx, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;

function TlrCheckListBox.CreateControl: TControl;
begin
  Result:=TCheckListBox.Create(nil);
end;

function TlrCheckListBox.ExecMetod(const AName: String; p1, p2, p3: Variant;
  var Val: Variant): boolean;
begin
  if AName = 'SETINDEXPROPERTY' then
  begin
    if p1 = 'CHECKED' then
      TCheckListBox(FControl).Checked[p2]:=p3
    else
    if p1 = 'ITEMS' then
      TCheckListBox(FControl).Items[p2]:=p3
  end
  else
  if AName = 'GETINDEXPROPERTY' then
  begin
    if p1 = 'CHECKED' then
      Val:=TCheckListBox(FControl).Checked[p2]
    else
    if p1 = 'ITEMS' then
      Val:=TCheckListBox(FControl).Items[p2]
  end
  else
  if AName = 'ITEMS.ADD' then
    Val:=TCheckListBox(FControl).Items.Add(frParser.Calc(P1))
  else
  if AName = 'ITEMS.DELETE' then
    TCheckListBox(FControl).Items.Delete(frParser.Calc(P1))
  else
  if AName = 'ITEMS.CLEAR' then
    TCheckListBox(FControl).Items.Clear
  else
  if AName = 'ITEMS.INDEXOF' then
    Val:=TCheckListBox(FControl).Items.IndexOf(frParser.Calc(P1))
end;

constructor TlrCheckListBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrCheckListBox';
  AutoSize:=true;
end;

procedure TlrCheckListBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
end;

procedure TlrCheckListBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
end;

procedure TlrCheckListBox.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrCheckListBox then
  begin
    Items.Assign(TlrCheckListBox(Source).Items);
    ItemIndex:=TlrCheckListBox(Source).ItemIndex;
  end;
end;

{ TlrButtonPanel }

function TlrButtonPanel.GetButtonOrder: TButtonOrder;
begin
  Result:= TButtonPanel(FControl).ButtonOrder;
end;

function TlrButtonPanel.GetShowButtons: TPanelButtons;
begin
  Result:= TButtonPanel(FControl).ShowButtons;
end;

procedure TlrButtonPanel.SetButtonOrder(AValue: TButtonOrder);
begin
  TButtonPanel(FControl).ButtonOrder:=AValue;
end;

procedure TlrButtonPanel.SetShowButtons(AValue: TPanelButtons);
begin
  TButtonPanel(FControl).ShowButtons:=AValue;
end;

procedure TlrButtonPanel.PaintDesignControl;
var
  AY, AX, aH, aW:integer;
  R1:TRect;
  i:TPanelButton;

  B:TPanelBitBtn;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);

  R1:=DRect;
  R1.Right:=R1.Right - 6;
  R1.Top:=R1.Top + 6;
  R1.Bottom:=R1.Bottom - 6;

  for i:=Low(TPanelButton) to High(TPanelButton) do
  begin
    if i in ShowButtons then
    begin

      case i of
        pbOK:B:=TButtonPanel(FControl).OKButton;
        pbCancel:B:=TButtonPanel(FControl).CancelButton;
        pbClose:B:=TButtonPanel(FControl).CloseButton;
        pbHelp:B:=TButtonPanel(FControl).HelpButton;
      else
        b:=nil;
      end;
      if Assigned(B) then
      begin
        R1.Left:=R1.Right - B.Width;
        DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);

        AX:=(R1.Left +  R1.Right) div 2;
        AY:=(R1.Top +  R1.Bottom) div 2;
        aW:=Canvas.TextWidth(B.Caption);
        aH:=Canvas.TextHeight(B.Caption) div 2;

        if aW>B.Width then
          Canvas.TextRect(R1, 0, AY - aH, B.Caption)
        else
          Canvas.TextRect(R1, AX - (aW div 2), AY - aH, B.Caption)

      end;
      R1.Right:=R1.Left - 6;
    end;
  end;
end;

function TlrButtonPanel.CreateControl: TControl;
begin
  Result:=TButtonPanel.Create(nil);
end;

procedure TlrButtonPanel.AfterCreate;
begin
  inherited AfterCreate;
  FControl.OnClick:=nil;
  TButtonPanel(FControl).OKButton.OnClick:=@OnClickHandle;
end;

constructor TlrButtonPanel.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrButtonPanel';
  AutoSize:=true;
  UpdateControlPosition;
end;

procedure TlrButtonPanel.UpdateControlPosition;
begin
  X:=2;
  if Assigned(OwnerPage) then
  begin
    Y:=OwnerPage.Height - FControl.Height - 4;
    Dx:=OwnerPage.Width-8;
  end
  else
  begin
    Y:=2;
    DX:=FControl.Width;
  end;
  Dy:=FControl.Height;
end;

procedure TlrButtonPanel.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('ButtonOrder',XML.GetValue(Path+'ButtonOrder', 'boDefault'));
  RestoreProperty('ShowButtons',XML.GetValue(Path+'ShowButtons', 'pbOK, pbCancel, pbClose, pbHelp'));
  UpdateControlPosition;
end;

procedure TlrButtonPanel.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'ButtonOrder', GetSaveProperty('ButtonOrder'));
  XML.SetValue(Path+'ShowButtons', GetSaveProperty('ShowButtons'));
end;

procedure TlrButtonPanel.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrButtonPanel then
  begin
    ButtonOrder:=TlrButtonPanel(Source).ButtonOrder;
    ShowButtons:=TlrButtonPanel(Source).ShowButtons;
  end;
end;

{ TlrDateEdit }

function TlrDateEdit.GetDate: TDateTime;
begin
  Result:=TDateEdit(FControl).Date;
end;

procedure TlrDateEdit.SetDate(AValue: TDateTime);
begin
  TDateEdit(FControl).Date:=AValue;
end;

procedure TlrDateEdit.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrDateEdit.CreateControl: TControl;
begin
  Result:=TDateEdit.Create(nil);
end;

constructor TlrDateEdit.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrDateEdit';
  AutoSize:=true;
end;

procedure TlrDateEdit.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
end;

procedure TlrDateEdit.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
end;

procedure TlrDateEdit.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrDateEdit then
    Date:=TlrDateEdit(Source).Date;
end;

{ TlrListBox }

function TlrListBox.GetItemIndex: integer;
begin
  Result:=TListBox(FControl).ItemIndex;
end;

function TlrListBox.GetItems: TStrings;
begin
  Result:=TListBox(FControl).Items;
end;

procedure TlrListBox.SetItemIndex(AValue: integer);
begin
  TListBox(FControl).ItemIndex:=AValue;
end;

procedure TlrListBox.SetItems(AValue: TStrings);
begin
  TListBox(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrListBox.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;
begin
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  AY:=DRect.Top + 1;
  i:=0;
  while (AY < DRect.Bottom) and (i<TListBox(FControl).Items.Count) do
  begin
    S:=TListBox(FControl).Items[i];
    Canvas.TextRect(DRect, DRect.Left + 3, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;

function TlrListBox.CreateControl: TControl;
begin
  Result:=TListBox.Create(nil);
end;

constructor TlrListBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrListBox';
  AutoSize:=true;
end;

procedure TlrListBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
end;

procedure TlrListBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
end;

procedure TlrListBox.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrListBox then
  begin
    Items.Assign(TlrListBox(Source).Items);
    ItemIndex:=TlrListBox(Source).ItemIndex;
  end;
end;

{ TlrMemo }

procedure TlrMemo.MemoChange(Sender: TObject);
begin
  if Assigned(FControl) then
    TMemo(FControl).Lines.Assign(Memo);
end;

procedure TlrMemo.PaintDesignControl;
var
  AY, aH, i:integer;
  S:string;
begin
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  AY:=DRect.Top + 1;
  i:=0;
  while (AY < DRect.Bottom) and (i<TMemo(FControl).Lines.Count) do
  begin
    S:=TMemo(FControl).Lines[i];
    Canvas.TextRect(DRect, DRect.Left + 3, AY, S);
    inc(AY, aH + 3);
    inc(i);
  end;
end;

function TlrMemo.CreateControl: TControl;
begin
  Result:=TMemo.Create(nil);
end;

function TlrMemo.GetText: string;
begin
  Result:=TMemo(FControl).Lines.Text;
end;

procedure TlrMemo.SetText(AValue: string);
begin
  TMemo(FControl).Lines.Text:=AValue;
end;

constructor TlrMemo.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrMemo';
  Memo.OnChange:=@MemoChange;
end;

procedure TlrMemo.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
end;

procedure TlrMemo.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
end;

{ TlrRadioButton }

function TlrRadioButton.CreateControl: TControl;
begin
  Result:=TRadioButton.Create(nil);
end;

function TlrRadioButton.GetCheckStyle(ACheck: boolean): TThemedButton;
begin
  if ACheck then
    Result:=tbRadioButtonCheckedNormal
  else
    Result:=tbRadioButtonUncheckedNormal
end;

constructor TlrRadioButton.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrRadioButton';
end;


{ TlrComboBox }

function TlrComboBox.GetDropDownCount: integer;
begin
  Result:=TComboBox(FControl).DropDownCount;
end;

function TlrComboBox.GetItemIndex: integer;
begin
  Result:=TComboBox(FControl).ItemIndex;
end;

function TlrComboBox.GetItems: TStrings;
begin
  Result:=TComboBox(FControl).Items;
end;

function TlrComboBox.GetStyle: TComboBoxStyle;
begin
  Result:=TComboBox(FControl).Style;
end;

procedure TlrComboBox.SetDropDownCount(AValue: integer);
begin
  TComboBox(FControl).DropDownCount:=AValue;
end;

procedure TlrComboBox.SetItemIndex(AValue: integer);
begin
  TComboBox(FControl).ItemIndex:=AValue;
end;

procedure TlrComboBox.SetItems(AValue: TStrings);
begin
  TComboBox(FControl).Items:=AValue;
  if Assigned(frDesigner) then
    frDesigner.Modified:=true;
end;

procedure TlrComboBox.SetStyle(AValue: TComboBoxStyle);
begin
  TComboBox(FControl).Style:=AValue;
end;

procedure TlrComboBox.PaintDesignControl;
var
  AY, aH:integer;
  R1:TRect;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text);

  R1:=DRect;
  R1.Left:=R1.Right - 16;
  DrawFrameControl(Canvas.Handle, R1, DFC_BUTTON, DFCS_BUTTONPUSH);
end;

function TlrComboBox.CreateControl: TControl;
begin
  Result:=TComboBox.Create(nil);
end;

constructor TlrComboBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrComboBox';
  AutoSize:=true;
end;

procedure TlrComboBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Items.Text:=XML.GetValue(Path+'Items/Text/Value'{%H-}, '');
  RestoreProperty('Style',XML.GetValue(Path+'Style/Value','csDropDown'));
  ItemIndex:=XML.GetValue(Path+'ItemIndex/Value'{%H-}, -1);
  DropDownCount:=XML.GetValue(Path+'DropDownCount/Value'{%H-}, DropDownCount);
end;

procedure TlrComboBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Items/Text/Value'{%H-}, Items.Text);
  XML.SetValue(Path+'Style/Value', GetSaveProperty('Style'));
  XML.SetValue(Path+'ItemIndex/Value'{%H-}, ItemIndex);
  XML.SetValue(Path+'DropDownCount/Value'{%H-}, DropDownCount);
end;

procedure TlrComboBox.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrComboBox then
  begin
    Style:=TlrComboBox(Source).Style;
    Items.Assign(TlrComboBox(Source).Items);
    ItemIndex:=TlrComboBox(Source).ItemIndex;
    DropDownCount:=TlrComboBox(Source).DropDownCount;
  end;
end;

{ TlrCheckBox }

type
  THackChekBox = class(TCustomCheckBox);

function TlrCheckBox.GetChecked: boolean;
begin
  Result:=THackChekBox(FControl).Checked;
end;

procedure TlrCheckBox.SetChecked(AValue: boolean);
begin
  THackChekBox(FControl).Checked:=AValue;
end;

procedure TlrCheckBox.PaintDesignControl;
var
  AX, AY, aW, aH:integer;
  CSize: TSize;
  PaintRect:TRect;
  details: TThemedElementDetails;
begin
  Details := ThemeServices.GetElementDetails(GetCheckStyle(Checked));
  CSize := ThemeServices.GetDetailSize(Details);
  PaintRect.Left := DRect.Left;
  PaintRect.Top  := (DRect.Top + DRect.Bottom - CSize.cy) div 2;
  PaintRect := Bounds(PaintRect.Left, PaintRect.Top, CSize.cx, CSize.cy);
  ThemeServices.DrawElement(Canvas.Handle, Details, PaintRect, nil);

  AX:=DRect.Left + CSize.cx + 2;
  AY:=(DRect.Top +  DRect.Bottom) div 2;
//  aW:=Canvas.TextWidth(Caption);
  aH:=Canvas.TextHeight(Caption) div 2;
  Canvas.TextRect(DRect, AX, AY - aH, Caption)
end;

function TlrCheckBox.CreateControl: TControl;
begin
  Result:=TCheckBox.Create(nil);
end;

function TlrCheckBox.GetCheckStyle(ACheck: boolean): TThemedButton;
begin
  if ACheck then
    Result:=tbCheckBoxCheckedNormal
  else
    Result:=tbCheckBoxUncheckedNormal
end;

constructor TlrCheckBox.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName:='lrCheckBox';
  AutoSize:=true;
end;

procedure TlrCheckBox.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Checked:=XML.GetValue(Path+'Checked/Value'{%H-}, false);
end;

procedure TlrCheckBox.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Checked/Value'{%H-}, Checked);
end;

procedure TlrCheckBox.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrCheckBox then
    Checked:=TlrCheckBox(Source).Checked;
end;

{ TlrButton }

function TlrButton.GetKind: TBitBtnKind;
begin
  Result:=TBitBtn(FControl).Kind;
end;

procedure TlrButton.SetKind(AValue: TBitBtnKind);
begin
  TBitBtn(FControl).Kind:=AValue;
end;

procedure TlrButton.PaintDesignControl;
var
  AX, AY, aW, aH:integer;
begin
  DrawFrameControl(Canvas.Handle, DRect, DFC_BUTTON, DFCS_BUTTONPUSH);
  AX:=(DRect.Left +  DRect.Right) div 2;
  AY:=(DRect.Top +  DRect.Bottom) div 2;
  aW:=Canvas.TextWidth(Caption);
  aH:=Canvas.TextHeight(Caption) div 2;
  if aW>Width then
    Canvas.TextRect(DRect, 0, AY - aH, Caption)
  else
    Canvas.TextRect(DRect, AX - (aW div 2), AY - aH, Caption)
end;

function TlrButton.CreateControl: TControl;
begin
  Result:=TBitBtn.Create(nil);
  Result.AutoSize:=true;
end;

constructor TlrButton.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrButton';
end;

procedure TlrButton.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  RestoreProperty('Kind',XML.GetValue(Path+'Kind/Value','bkCustom'));
end;

procedure TlrButton.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Kind/Value', GetSaveProperty('Kind'));
end;

procedure TlrButton.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrButton then
    Kind:=TlrButton(Source).Kind;
end;

{ TlrEdit }

procedure TlrEdit.PaintDesignControl;
var
  AY, aH:integer;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Frame3d(DRect, 1, bvLowered);
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text)
end;

function TlrEdit.CreateControl: TControl;
begin
  Result:=TEdit.Create(nil);
end;

constructor TlrEdit.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrEdit';
end;

procedure TlrEdit.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Text:=XML.GetValue(Path+'Text/Value'{%H-}, '');
end;

procedure TlrEdit.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Text/Value'{%H-}, Text);
end;

{ TlrLabel }

function TlrLabel.GetAlignment: TAlignment;
begin
  Result:=TLabel(FControl).Alignment;
end;

function TlrLabel.GetWordWrap: boolean;
begin
  Result:=TLabel(FControl).WordWrap;
end;

procedure TlrLabel.SetAlignment(AValue: TAlignment);
begin
  TLabel(FControl).Alignment:=AValue;
end;

procedure TlrLabel.SetWordWrap(AValue: boolean);
begin
  TLabel(FControl).WordWrap:=AValue;
end;

procedure TlrLabel.PaintDesignControl;
var
  AY, aH:integer;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Brush.Color := FControl.Color;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Text)
end;

function TlrLabel.CreateControl: TControl;
begin
  Result:=TLabel.Create(nil);
end;


constructor TlrLabel.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  BaseName := 'lrLabel';
end;

procedure TlrLabel.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  WordWrap:=XML.GetValue(Path+'WordWrap/Value'{%H-}, false);
  RestoreProperty('Alignment',XML.GetValue(Path+'Alignment/Value',''));
end;

procedure TlrLabel.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'WordWrap/Value'{%H-}, WordWrap);
  XML.SetValue(Path+'Alignment/Value', GetSaveProperty('Alignment'));
end;

procedure TlrLabel.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrLabel then
  begin
    Alignment:=TlrLabel(Source).Alignment;
    WordWrap:=TlrLabel(Source).WordWrap;
  end;
end;


{ TlrVisualControl }

procedure TlrVisualControl.SetColor(AValue: TColor);
begin
  FControl.Color:=AValue;
  Invalidate;
end;

function TlrVisualControl.GetAutoSize: Boolean;
begin
  Result:=FControl.AutoSize;
end;

function TlrVisualControl.GetCaption: string;
begin
  Result:=FControl.Caption;
end;

function TlrVisualControl.GetColor: TColor;
begin
  Result:=FControl.Color;
end;

function TlrVisualControl.GetEnabled: boolean;
begin
  Result:=FControl.Enabled;
end;

function TlrVisualControl.GetFont: TFont;
begin
  Result:=FControl.Font;
end;

function TlrVisualControl.GetHint: string;
begin
  Result:=FControl.Hint;
end;

function TlrVisualControl.GetOnClick: TfrScriptStrings;
begin
  Result:=Script;
end;

procedure TlrVisualControl.SetAutoSize(AValue: Boolean);
begin
  FControl.AutoSize:=AValue;
end;

procedure TlrVisualControl.SetCaption(AValue: string);
begin
  FControl.Caption:=AValue;
{  DX:=FControl.Width;
  DY:=FControl.Height;}
  Invalidate;
end;

procedure TlrVisualControl.SetEnabled(AValue: boolean);
begin
  FControl.Enabled:=AValue;
  Invalidate;
end;

procedure TlrVisualControl.SetFont(AValue: TFont);
begin
  FControl.Font:=AValue;
  Invalidate;
end;

procedure TlrVisualControl.OnClickHandle(Sender: TObject);
var
  FSaveView:TfrView;
  FSavePage:TfrPage;
  CmdList, ErrorList:TStringList;
begin
  if (DocMode = dmPrinting) then
  begin
    if (Script.Count>0) and (Trim(Script.Text)<>'') and (Assigned(CurReport))then
    begin
      FSaveView:=CurView;
      FSavePage:=CurPage;
      CmdList:=TStringList.Create;
      ErrorList:=TStringList.Create;
      try
        CurView := Self;
        CurPage:=OwnerPage;
        frInterpretator.PrepareScript(Script, CmdList, ErrorList);
        frInterpretator.DoScript(CmdList);
      finally
        CurPage:=FSavePage;
        CurView := FSaveView;
        FreeAndNil(CmdList);
        FreeAndNil(ErrorList);
      end;
    end;
    if (PSOnClick <> '') and Assigned(CurReport) then
      PSExecOnClick(PSOnClick, Self);
  end;
end;

procedure TlrVisualControl.SetHint(AValue: string);
begin
  FControl.Hint:=AValue;
end;

procedure TlrVisualControl.SetOnClick(AValue: TfrScriptStrings);
begin
  Script:=AValue;
end;

procedure TlrVisualControl.SetVisible(AValue: Boolean);
begin
  inherited SetVisible(AValue);
  FControl.Visible:=AValue;
end;

procedure TlrVisualControl.PaintDesignControl;
var
  AY, aH:integer;
  S:string;
begin
  AY:=(DRect.Top + DRect.Bottom) div 2;
  aH:=Canvas.TextHeight(Text) div 2;
  Canvas.Brush.Color := clRed;
  Canvas.FillRect(DRect);
  Canvas.Font:=FControl.Font;
  Canvas.TextRect(DRect, DRect.Left + 3, AY - aH, Name)
end;

procedure TlrVisualControl.SetName(const AValue: string);
begin
  inherited SetName(AValue);
  FControl.Name:=Name;
end;

procedure TlrVisualControl.UpdateControlPosition;
begin
  FControl.Left:=round(Left);
  FControl.Top:=round(Top) - 20; //Header width
  FControl.Width:=round(Width);
  FControl.Height:=round(Height);
end;

procedure TlrVisualControl.AttachToParent;
begin
  FControl.Parent := OwnerForm;
end;

procedure TlrVisualControl.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TlrVisualControl then
  begin
    AutoSize:=TlrVisualControl(Source).AutoSize;
    Color:=TlrVisualControl(Source).Color;
    Caption:=TlrVisualControl(Source).Caption;
    Text:=TlrVisualControl(Source).Text;
    Font:=TlrVisualControl(Source).Font;
    Hint:=TlrVisualControl(Source).Hint;
    OnClick:=TlrVisualControl(Source).OnClick;
  end;
end;

procedure TlrVisualControl.AfterCreate;
begin
  inherited AfterCreate;
  if Assigned(FControl) then
    FControl.OnClick:=@OnClickHandle;
end;

procedure TlrVisualControl.PSExecOnClick(ProcName: String;
  VControl: TlrVisualControl);
var
  Params: TPSList;
  Proc: Cardinal;
  Arg: TPSVariantClass;
begin
  Proc := CurReport.PSScript.Exec.GetProc(ProcName);
  Params := TPSList.Create;
  Arg.VI.FType := CurReport.PSScript.Exec.FindType2(btClass);
  Arg.Data := VControl;
  Params.Add(@Arg);
  CurReport.PSScript.Exec.RunProc(Params, Proc);
  Params.Free;
end;

constructor TlrVisualControl.Create(AOwnerPage: TfrPage);
begin
  inherited Create(AOwnerPage);
  FControl:=CreateControl;
  if Assigned(FControl) then
  begin
    x:=FControl.Left;
    Y:=FControl.Top;
    DX:=FControl.Width;
    DY:=FControl.Height;
  end;
end;

destructor TlrVisualControl.Destroy;
begin
  FControl.Free;
  FControl := nil;
  inherited Destroy;
end;

procedure TlrVisualControl.LoadFromXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited LoadFromXML(XML, Path);
  Enabled:=XML.GetValue(Path+'Enabled/Value'{%H-}, true);
  Font.Name := XML.GetValue(Path+'Font/Name/Value', 'Arial'); // todo chk
  Font.Size := XML.GetValue(Path+'Font/Size/Value'{%H-}, 10); // todo chk
  RestoreProperty('CharSet',XML.GetValue(Path+'Font/Charset/Value',''),Font);
  RestoreProperty('Style',XML.GetValue(Path+'Font/Style/Value',''),Font);
  Font.Color := StringToColor(XML.GetValue(Path+'Font/Color/Value','clBlack')); // todo chk
  Caption:=XML.GetValue(Path+'Caption/Value'{%H-}, '');
  AutoSize:=XML.GetValue(Path+'AutoSize/Value'{%H-}, true);
  Color:= StringToColor(XML.GetValue(Path+'Color/Value', 'clNone'));
  Hint:=XML.GetValue(Path+'Hint/Value'{%H-}, '');
  if StreamMode = smDesigning then
  begin
    OnClick.Text:=XML.GetValue(Path+'Event/OnClick/Value', '');
  end;
  PSOnClick:=XML.GetValue(Path+'Event/PSOnClick/Value', '');
end;

procedure TlrVisualControl.SaveToXML(XML: TLrXMLConfig; const Path: String);
begin
  inherited SaveToXML(XML, Path);
  XML.SetValue(Path+'Enabled/Value'{%H-}, Enabled);
  XML.SetValue(Path+'Font/Name/Value', Font.Name);
  XML.SetValue(Path+'Font/Size/Value'{%H-}, Font.Size);
  XML.SetValue(Path+'Font/Color/Value', ColorToString(Font.Color));
  XML.SetValue(Path+'Font/Charset/Value', GetSaveProperty('CharSet',Font));
  XML.SetValue(Path+'Font/Style/Value', GetSaveProperty('Style',Font));
  XML.SetValue(Path+'Caption/Value'{%H-}, Caption);
  XML.SetValue(Path+'AutoSize/Value'{%H-}, AutoSize);
  XML.SetValue(Path+'Color/Value', ColorToString(Color));
  XML.SetValue(Path+'Hint/Value'{%H-}, Hint);

  if StreamMode = smDesigning then
  begin
    if IsPublishedProp(self,'OnClick') then
      XML.SetValue(Path+'Event/OnClick/Value', OnClick.Text);
  end;
  if IsPublishedProp(self,'PSOnClick') then
    XML.SetValue(Path+'Event/PSOnClick/Value', PSOnClick);
end;

(* === compile-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrRadioGroup(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrRadioGroup') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrRadioGroup') do
  begin
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'integer', iptrw);
    RegisterProperty('ItemsCount', 'integer', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrCheckListBox(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrCheckListBox') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrCheckListBox') do
  begin
    RegisterProperty('Checked', 'Boolean Integer', iptrw);
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'integer', iptrw);
    RegisterProperty('ItemsCount', 'integer', iptr);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrButtonPanel(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrButtonPanel') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrButtonPanel') do
  begin
    RegisterProperty('ButtonOrder', 'TButtonOrder', iptrw);
    RegisterProperty('ShowButtons', 'TPanelButtons', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrDateEdit(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrDateEdit') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrDateEdit') do
  begin
    RegisterProperty('Date', 'TDateTime', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrComboBox(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrComboBox') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrComboBox') do
  begin
    RegisterProperty('Style', 'TComboBoxStyle', iptrw);
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'integer', iptrw);
    RegisterProperty('DropDownCount', 'integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrListBox(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrListBox') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrListBox') do
  begin
    RegisterProperty('Items', 'TStrings', iptrw);
    RegisterProperty('ItemIndex', 'integer', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrRadioButton(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrCheckBox', 'TlrRadioButton') do
  with CL.AddClassN(CL.FindClass('TlrCheckBox'),'TlrRadioButton') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrCheckBox(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrCheckBox') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrCheckBox') do
  begin
    RegisterProperty('Checked', 'boolean', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrButton(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrButton') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrButton') do
  begin
    RegisterProperty('Kind', 'TBitBtnKind', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrMemo(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrMemo') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrMemo') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrEdit(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrEdit') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrEdit') do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrLabel(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TlrVisualControl', 'TlrLabel') do
  with CL.AddClassN(CL.FindClass('TlrVisualControl'),'TlrLabel') do
  begin
    RegisterProperty('Alignment', 'TAlignment', iptrw);
    RegisterProperty('WordWrap', 'boolean', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_TlrVisualControl(CL: TPSPascalCompiler);
begin
  //with RegClassS(CL,'TfrControl', 'TlrVisualControl') do
  with CL.AddClassN(CL.FindClass('TfrControl'),'TlrVisualControl') do
  begin
    RegisterProperty('Control', 'TControl', iptrw);
    RegisterProperty('AutoSize', 'Boolean', iptrw);
    RegisterProperty('Color', 'TColor', iptrw);
    RegisterProperty('Caption', 'string', iptrw);
    RegisterProperty('Text', 'string', iptrw);
    RegisterProperty('Font', 'TFont', iptrw);
    RegisterProperty('Hint', 'string', iptrw);
    RegisterProperty('OnClick', 'TfrScriptStrings', iptrw);
    RegisterProperty('Enabled', 'boolean', iptrw);
  end;
end;

(*----------------------------------------------------------------------------*)
procedure SIRegister_LRDialogControls(CL: TPSPascalCompiler);
begin
  SIRegister_TlrVisualControl(CL);
  //CL.AddTypeS('TlrVisualControlClass', 'class of TlrVisualControl');
  SIRegister_TlrLabel(CL);
  SIRegister_TlrEdit(CL);
  SIRegister_TlrMemo(CL);
  SIRegister_TlrButton(CL);
  SIRegister_TlrCheckBox(CL);
  SIRegister_TlrRadioButton(CL);
  SIRegister_TlrListBox(CL);
  SIRegister_TlrComboBox(CL);
  SIRegister_TlrDateEdit(CL);
  SIRegister_TlrButtonPanel(CL);
  SIRegister_TlrCheckListBox(CL);
  SIRegister_TlrRadioGroup(CL);
end;

(* === run-time registration functions === *)
(*----------------------------------------------------------------------------*)
procedure TlrRadioGroupItemsCount_R(Self: TlrRadioGroup; var T: integer);
begin T := Self.ItemsCount; end;

(*----------------------------------------------------------------------------*)
procedure TlrRadioGroupItemIndex_W(Self: TlrRadioGroup; const T: integer);
begin Self.ItemIndex := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrRadioGroupItemIndex_R(Self: TlrRadioGroup; var T: integer);
begin T := Self.ItemIndex; end;

(*----------------------------------------------------------------------------*)
procedure TlrRadioGroupItems_W(Self: TlrRadioGroup; const T: TStrings);
begin Self.Items := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrRadioGroupItems_R(Self: TlrRadioGroup; var T: TStrings);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxChecked_W(Self: TlrCheckListBox; const T: Boolean; const t1: Integer);
begin Self.Checked[t1] := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxChecked_R(Self: TlrCheckListBox; var T: Boolean; const t1: Integer);
begin T := Self.Checked[t1]; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxItemsCount_R(Self: TlrCheckListBox; var T: integer);
begin T := Self.ItemsCount; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxItemIndex_W(Self: TlrCheckListBox; const T: integer);
begin Self.ItemIndex := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxItemIndex_R(Self: TlrCheckListBox; var T: integer);
begin T := Self.ItemIndex; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxItems_W(Self: TlrCheckListBox; const T: TStrings);
begin Self.Items := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckListBoxItems_R(Self: TlrCheckListBox; var T: TStrings);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonPanelShowButtons_W(Self: TlrButtonPanel; const T: TPanelButtons);
begin Self.ShowButtons := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonPanelShowButtons_R(Self: TlrButtonPanel; var T: TPanelButtons);
begin T := Self.ShowButtons; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonPanelButtonOrder_W(Self: TlrButtonPanel; const T: TButtonOrder);
begin Self.ButtonOrder := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonPanelButtonOrder_R(Self: TlrButtonPanel; var T: TButtonOrder);
begin T := Self.ButtonOrder; end;

(*----------------------------------------------------------------------------*)
procedure TlrDateEditDate_W(Self: TlrDateEdit; const T: TDateTime);
begin Self.Date := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrDateEditDate_R(Self: TlrDateEdit; var T: TDateTime);
begin T := Self.Date; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxDropDownCount_W(Self: TlrComboBox; const T: integer);
begin Self.DropDownCount := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxDropDownCount_R(Self: TlrComboBox; var T: integer);
begin T := Self.DropDownCount; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxItemIndex_W(Self: TlrComboBox; const T: integer);
begin Self.ItemIndex := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxItemIndex_R(Self: TlrComboBox; var T: integer);
begin T := Self.ItemIndex; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxItems_W(Self: TlrComboBox; const T: TStrings);
begin Self.Items := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxItems_R(Self: TlrComboBox; var T: TStrings);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxStyle_W(Self: TlrComboBox; const T: TComboBoxStyle);
begin Self.Style := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrComboBoxStyle_R(Self: TlrComboBox; var T: TComboBoxStyle);
begin T := Self.Style; end;

(*----------------------------------------------------------------------------*)
procedure TlrListBoxItemIndex_W(Self: TlrListBox; const T: integer);
begin Self.ItemIndex := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrListBoxItemIndex_R(Self: TlrListBox; var T: integer);
begin T := Self.ItemIndex; end;

(*----------------------------------------------------------------------------*)
procedure TlrListBoxItems_W(Self: TlrListBox; const T: TStrings);
begin Self.Items := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrListBoxItems_R(Self: TlrListBox; var T: TStrings);
begin T := Self.Items; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckBoxChecked_W(Self: TlrCheckBox; const T: boolean);
begin Self.Checked := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrCheckBoxChecked_R(Self: TlrCheckBox; var T: boolean);
begin T := Self.Checked; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonKind_W(Self: TlrButton; const T: TBitBtnKind);
begin Self.Kind := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrButtonKind_R(Self: TlrButton; var T: TBitBtnKind);
begin T := Self.Kind; end;

(*----------------------------------------------------------------------------*)
procedure TlrLabelWordWrap_W(Self: TlrLabel; const T: boolean);
begin Self.WordWrap := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrLabelWordWrap_R(Self: TlrLabel; var T: boolean);
begin T := Self.WordWrap; end;

(*----------------------------------------------------------------------------*)
procedure TlrLabelAlignment_W(Self: TlrLabel; const T: TAlignment);
begin Self.Alignment := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrLabelAlignment_R(Self: TlrLabel; var T: TAlignment);
begin T := Self.Alignment; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlEnabled_W(Self: TlrVisualControl; const T: boolean);
begin Self.Enabled := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlEnabled_R(Self: TlrVisualControl; var T: boolean);
begin T := Self.Enabled; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlOnClick_W(Self: TlrVisualControl; const T: TfrScriptStrings);
begin Self.OnClick := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlOnClick_R(Self: TlrVisualControl; var T: TfrScriptStrings);
begin T := Self.OnClick; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlHint_W(Self: TlrVisualControl; const T: string);
begin Self.Hint := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlHint_R(Self: TlrVisualControl; var T: string);
begin T := Self.Hint; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlFont_W(Self: TlrVisualControl; const T: TFont);
begin Self.Font := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlFont_R(Self: TlrVisualControl; var T: TFont);
begin T := Self.Font; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlText_W(Self: TlrVisualControl; const T: string);
begin Self.Text := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlText_R(Self: TlrVisualControl; var T: string);
begin T := Self.Text; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlCaption_W(Self: TlrVisualControl; const T: string);
begin Self.Caption := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlCaption_R(Self: TlrVisualControl; var T: string);
begin T := Self.Caption; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlColor_W(Self: TlrVisualControl; const T: TColor);
begin Self.Color := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlColor_R(Self: TlrVisualControl; var T: TColor);
begin T := Self.Color; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlAutoSize_W(Self: TlrVisualControl; const T: Boolean);
begin Self.AutoSize := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlAutoSize_R(Self: TlrVisualControl; var T: Boolean);
begin T := Self.AutoSize; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlControl_W(Self: TlrVisualControl; const T: TControl);
begin Self.Control := T; end;

(*----------------------------------------------------------------------------*)
procedure TlrVisualControlControl_R(Self: TlrVisualControl; var T: TControl);
begin T := Self.Control; end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrRadioGroup(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrRadioGroup) do
  begin
    RegisterPropertyHelper(@TlrRadioGroupItems_R,@TlrRadioGroupItems_W,'Items');
    RegisterPropertyHelper(@TlrRadioGroupItemIndex_R,@TlrRadioGroupItemIndex_W,'ItemIndex');
    RegisterPropertyHelper(@TlrRadioGroupItemsCount_R,nil,'ItemsCount');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrCheckListBox(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrCheckListBox) do
  begin
    RegisterPropertyHelper(@TlrCheckListBoxChecked_R,@TlrCheckListBoxChecked_W,'Checked');
    RegisterPropertyHelper(@TlrCheckListBoxItems_R,@TlrCheckListBoxItems_W,'Items');
    RegisterPropertyHelper(@TlrCheckListBoxItemIndex_R,@TlrCheckListBoxItemIndex_W,'ItemIndex');
    RegisterPropertyHelper(@TlrCheckListBoxItemsCount_R,nil,'ItemsCount');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrButtonPanel(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrButtonPanel) do
  begin
    RegisterPropertyHelper(@TlrButtonPanelButtonOrder_R,@TlrButtonPanelButtonOrder_W,'ButtonOrder');
    RegisterPropertyHelper(@TlrButtonPanelShowButtons_R,@TlrButtonPanelShowButtons_W,'ShowButtons');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrDateEdit(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrDateEdit) do
  begin
    RegisterPropertyHelper(@TlrDateEditDate_R,@TlrDateEditDate_W,'Date');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrComboBox(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrComboBox) do
  begin
    RegisterPropertyHelper(@TlrComboBoxStyle_R,@TlrComboBoxStyle_W,'Style');
    RegisterPropertyHelper(@TlrComboBoxItems_R,@TlrComboBoxItems_W,'Items');
    RegisterPropertyHelper(@TlrComboBoxItemIndex_R,@TlrComboBoxItemIndex_W,'ItemIndex');
    RegisterPropertyHelper(@TlrComboBoxDropDownCount_R,@TlrComboBoxDropDownCount_W,'DropDownCount');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrListBox(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrListBox) do
  begin
    RegisterPropertyHelper(@TlrListBoxItems_R,@TlrListBoxItems_W,'Items');
    RegisterPropertyHelper(@TlrListBoxItemIndex_R,@TlrListBoxItemIndex_W,'ItemIndex');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrRadioButton(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrRadioButton) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrCheckBox(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrCheckBox) do
  begin
    RegisterPropertyHelper(@TlrCheckBoxChecked_R,@TlrCheckBoxChecked_W,'Checked');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrButton(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrButton) do
  begin
    RegisterPropertyHelper(@TlrButtonKind_R,@TlrButtonKind_W,'Kind');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrMemo(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrMemo) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrEdit(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrEdit) do
  begin
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrLabel(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrLabel) do
  begin
    RegisterPropertyHelper(@TlrLabelAlignment_R,@TlrLabelAlignment_W,'Alignment');
    RegisterPropertyHelper(@TlrLabelWordWrap_R,@TlrLabelWordWrap_W,'WordWrap');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_TlrVisualControl(CL: TPSRuntimeClassImporter);
begin
  with CL.Add(TlrVisualControl) do
  begin
    RegisterPropertyHelper(@TlrVisualControlControl_R,@TlrVisualControlControl_W,'Control');
    RegisterPropertyHelper(@TlrVisualControlAutoSize_R,@TlrVisualControlAutoSize_W,'AutoSize');
    RegisterPropertyHelper(@TlrVisualControlColor_R,@TlrVisualControlColor_W,'Color');
    RegisterPropertyHelper(@TlrVisualControlCaption_R,@TlrVisualControlCaption_W,'Caption');
    RegisterPropertyHelper(@TlrVisualControlText_R,@TlrVisualControlText_W,'Text');
    RegisterPropertyHelper(@TlrVisualControlFont_R,@TlrVisualControlFont_W,'Font');
    RegisterPropertyHelper(@TlrVisualControlHint_R,@TlrVisualControlHint_W,'Hint');
    RegisterPropertyHelper(@TlrVisualControlOnClick_R,@TlrVisualControlOnClick_W,'OnClick');
    RegisterPropertyHelper(@TlrVisualControlEnabled_R,@TlrVisualControlEnabled_W,'Enabled');
  end;
end;

(*----------------------------------------------------------------------------*)
procedure RIRegister_LRDialogControls(CL: TPSRuntimeClassImporter);
begin
  RIRegister_TlrVisualControl(CL);
  RIRegister_TlrLabel(CL);
  RIRegister_TlrEdit(CL);
  RIRegister_TlrMemo(CL);
  RIRegister_TlrButton(CL);
  RIRegister_TlrCheckBox(CL);
  RIRegister_TlrRadioButton(CL);
  RIRegister_TlrListBox(CL);
  RIRegister_TlrComboBox(CL);
  RIRegister_TlrDateEdit(CL);
  RIRegister_TlrButtonPanel(CL);
  RIRegister_TlrCheckListBox(CL);
  RIRegister_TlrRadioGroup(CL);
end;

(*----------------------------------------------------------------------------*)

procedure PSCompImportDialogControlsTlrLabel(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrVisualControl(APSComp);
  SIRegister_TlrLabel(APSComp);
end;

procedure PSCompImportDialogControlsTlrEdit(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrEdit(APSComp);
end;

procedure PSCompImportDialogControlsTlrMemo(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrMemo(APSComp);
end;

procedure PSCompImportDialogControlsTlrButton(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrButton(APSComp);
end;

procedure PSCompImportDialogControlsTlrCheckBox(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrCheckBox(APSComp);
end;

procedure PSCompImportDialogControlsTlrRadioButton(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrRadioButton(APSComp);
end;

procedure PSCompImportDialogControlsTlrListBox(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrListBox(APSComp);
end;

procedure PSCompImportDialogControlsTlrComboBox(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrComboBox(APSComp);
end;

procedure PSCompImportDialogControlsTlrDateEdit(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrDateEdit(APSComp);
end;

procedure PSCompImportDialogControlsTlrButtonPanel(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrButtonPanel(APSComp);
end;

procedure PSCompImportDialogControlsTlrCheckListBox(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrCheckListBox(APSComp);
end;

procedure PSCompImportDialogControlsTlrRadioGroup(APSComp: TPSPascalCompiler);
begin
  SIRegister_TlrRadioGroup(APSComp);
end;

procedure PSExecImportDialogControlsTlrLabel(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrVisualControl(APSImporter);
  RIRegister_TlrLabel(APSImporter);
end;

procedure PSExecImportDialogControlsTlrEdit(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrEdit(APSImporter);
end;

procedure PSExecImportDialogControlsTlrMemo(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrMemo(APSImporter);
end;

procedure PSExecImportDialogControlsTlrButton(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrButton(APSImporter);
end;

procedure PSExecImportDialogControlsTlrCheckBox(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrCheckBox(APSImporter);
end;

procedure PSExecImportDialogControlsTlrRadioButton(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrRadioButton(APSImporter);
end;

procedure PSExecImportDialogControlsTlrListBox(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrListBox(APSImporter);
end;

procedure PSExecImportDialogControlsTlrComboBox(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrComboBox(APSImporter);
end;

procedure PSExecImportDialogControlsTlrDateEdit(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrDateEdit(APSImporter);
end;

procedure PSExecImportDialogControlsTlrButtonPanel(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrButtonPanel(APSImporter);
end;

procedure PSExecImportDialogControlsTlrCheckListBox(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrCheckListBox(APSImporter);
end;

procedure PSExecImportDialogControlsTlrRadioGroup(APSExec: TPSExec; APSImporter: TPSRuntimeClassImporter);
begin
  RIRegister_TlrRadioGroup(APSImporter);
end;

procedure DoRegsiterControl(var cmpBMP:TBitmap; lrClass:TlrVisualControlClass;
  PSCompImportProc: TfrPSCompImport; PSExecImportProc: TfrPSExecImport);
begin
  if not assigned(cmpBMP) then
  begin
    cmpBMP := TBitmap.Create;
    cmpBMP.LoadFromResourceName(HInstance, lrClass.ClassName);
    frRegisterObject(lrClass, cmpBMP, lrClass.ClassName, nil, otlUIControl, nil,
      nil, PSCompImportProc, PSExecImportProc);
  end;
end;

procedure InitLRComp;
begin
  DoRegsiterControl(lrBMP_LRLabel, TlrLabel, @PSCompImportDialogControlsTlrLabel, @PSExecImportDialogControlsTlrLabel);
  DoRegsiterControl(lrBMP_LREdit, TlrEdit, @PSCompImportDialogControlsTlrEdit, @PSExecImportDialogControlsTlrEdit);
  DoRegsiterControl(lrBMP_LRMemo, TlrMemo, @PSCompImportDialogControlsTlrMemo, @PSExecImportDialogControlsTlrMemo);
  DoRegsiterControl(lrBMP_LRButton, TlrButton, @PSCompImportDialogControlsTlrButton, @PSExecImportDialogControlsTlrButton);
  DoRegsiterControl(lrBMP_LRCheckBox, TlrCheckBox, @PSCompImportDialogControlsTlrCheckBox, @PSExecImportDialogControlsTlrCheckBox);
  DoRegsiterControl(lrBMP_LRComboBox, TlrComboBox, @PSCompImportDialogControlsTlrComboBox, @PSExecImportDialogControlsTlrComboBox);
  DoRegsiterControl(lrBMP_LRRadioButton, TlrRadioButton, @PSCompImportDialogControlsTlrRadioButton, @PSExecImportDialogControlsTlrRadioButton);
  DoRegsiterControl(lrBMP_LRListBox, TlrListBox, @PSCompImportDialogControlsTlrListBox, @PSExecImportDialogControlsTlrListBox);
  DoRegsiterControl(lrBMP_LRDateEdit, TlrDateEdit, @PSCompImportDialogControlsTlrDateEdit, @PSExecImportDialogControlsTlrDateEdit);
  DoRegsiterControl(lrBMP_LRButtonPanel, TlrButtonPanel, @PSCompImportDialogControlsTlrButtonPanel, @PSExecImportDialogControlsTlrButtonPanel);
  DoRegsiterControl(lrBMP_LRCheckListBox, TlrCheckListBox, @PSCompImportDialogControlsTlrCheckListBox, @PSExecImportDialogControlsTlrCheckListBox);
  DoRegsiterControl(lrBMP_LRRadioGroupBox, TlrRadioGroup, @PSCompImportDialogControlsTlrRadioGroup, @PSExecImportDialogControlsTlrRadioGroup);
end;

procedure DoneLRComp;
begin
  if Assigned(lrBMP_LRLabel) then
    FreeAndNil(lrBMP_LRLabel);
  if Assigned(lrBMP_LREdit) then
    FreeAndNil(lrBMP_LREdit);
  if Assigned(lrBMP_LRButton) then
    FreeAndNil(lrBMP_LRButton);
  if Assigned(lrBMP_LRCheckBox) then
    FreeAndNil(lrBMP_LRCheckBox);
  if Assigned(lrBMP_LRComboBox) then
    FreeAndNil(lrBMP_LRComboBox);
  if Assigned(lrBMP_LRRadioButton) then
    FreeAndNil(lrBMP_LRRadioButton);
  if Assigned(lrBMP_LRMemo) then
    FreeAndNil(lrBMP_LRMemo);
  if Assigned(lrBMP_LRListBox) then
    FreeAndNil(lrBMP_LRListBox);
  if Assigned(lrBMP_LRDateEdit) then
    FreeAndNil(lrBMP_LRDateEdit);
  if Assigned(lrBMP_LRButtonPanel) then
    FreeAndNil(lrBMP_LRButtonPanel);
  if Assigned(lrBMP_LRCheckListBox) then
    FreeAndNil(lrBMP_LRCheckListBox);
  if Assigned(lrBMP_LRRadioGroupBox) then
    FreeAndNil(lrBMP_LRRadioGroupBox);
end;

initialization
  InitLRComp;

finalization
  DoneLRComp;
end.



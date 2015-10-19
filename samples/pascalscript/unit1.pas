unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, LR_Class, LR_BarC, LR_RRect,
  LR_Shape, LR_ChBox, LR_Desgn, LRDialogControls, LR_IBConnection, Forms,
  Controls, Graphics, Dialogs, ExtCtrls, EditBtn, StdCtrls;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Edit1: TEdit;
    Edit2: TEdit;
    FileNameEdit1: TFileNameEdit;
    frBarCodeObject1: TfrBarCodeObject;
    frCheckBoxObject1: TfrCheckBoxObject;
    frCompositeReport1: TfrCompositeReport;
    frDesigner1: TfrDesigner;
    frReport1: TfrReport;
    frReport2: TfrReport;
    frReport3: TfrReport;
    frRoundRectObject1: TfrRoundRectObject;
    frShapeObject1: TfrShapeObject;
    IBConnection1: TIBConnection;
    LRDialogControls1: TLRDialogControls;
    LR_IBConnection1: TLR_IBConnection;
    RadioGroup1: TRadioGroup;
    SQLTransaction1: TSQLTransaction;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
begin
  IBConnection1.Connected := False;
  IBConnection1.DatabaseName := FileNameEdit1.FileName;
  IBConnection1.UserName := Edit1.Text;
  IBConnection1.Password := Edit2.Text;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  if RadioGroup1.ItemIndex = RadioGroup1.Items.Count - 1 then
  begin
    frCompositeReport1.Reports.Clear;
    frCompositeReport1.Reports.Add(frReport1);
    frCompositeReport1.Reports.Add(frReport2);
    frCompositeReport1.Reports.Add(frReport3);
    frCompositeReport1.ShowReport;
  end;
  case RadioGroup1.ItemIndex of
    0: frReport1.ShowReport;
    1: frReport2.ShowReport;
    2: frReport3.ShowReport;
  end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  case RadioGroup1.ItemIndex of
    0: frReport1.DesignReport;
    1: frReport2.DesignReport;
    2: frReport3.DesignReport;
  end;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
  Close;
end;

end.


unit MainU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, ViewOnlyU, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TFormMain = class(TForm)
    Button1: TButton;
    Edit1: TEdit;
    RichEdit1: TRichEdit;
    CheckBox1: TCheckBox;
    Button2: TButton;
    ListBox1: TListBox;
    ScrollBar1: TScrollBar;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

procedure TFormMain.Button1Click(Sender: TObject);
var
  i: Integer;
const
  Captions: array [boolean] of string = ('Off', 'On');
begin
  Button1.Caption := 'View Only ' + Captions[Button2.ViewOnly];
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TWinControl then
      TWinControl(Components[i]).ViewOnly := not TWinControl(Components[i]).ViewOnly;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Button1.CanViewOnly := false;
end;

end.

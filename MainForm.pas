unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,FolderWatch;

type
  TFormMain = class(TForm)
    ListBox1: TListBox;
    Panel1: TPanel;
    Edit1: TEdit;
    Button1: TButton;
    ListBox2: TListBox;
    ListBox3: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private êÈåæ }
    FFolderWatch : TFolderWatch;
    procedure ShowList(const  Caption : string;List : TListBox;FileNames : TStringList);
    procedure OnFileChange(Sender: TObject; const AddNames: TStringList; const DelNames: TStringList; const ChangeNames: TStringList);
  public
    { Public êÈåæ }
  end;

var
  FormMain: TFormMain;

implementation

uses  System.IOUtils, Vcl.FileCtrl;

{$R *.dfm}

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Edit1.Text := TPath.GetPicturesPath;
  FFolderWatch := TFolderWatch.Create;
  FFolderWatch.FolderPath := Edit1.Text;
  FFolderWatch.FirstScanDone := true;
  FFolderWatch.OnFileChange := OnFileChange;
  FFolderWatch.Start;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  FFolderWatch.Free;
end;

procedure TFormMain.FormShow(Sender: TObject);
begin
  //
end;

procedure TFormMain.OnFileChange(Sender: TObject; const AddNames, DelNames,
  ChangeNames: TStringList);
begin
  if AddNames.Count > 0 then  ShowList('í«â¡',ListBox1,AddNames);
  if DelNames.Count > 0 then ShowList('çÌèú',ListBox2,DelNames);
  if ChangeNames.Count > 0 then ShowList('ïœçX',ListBox3,ChangeNames);
end;

procedure TFormMain.ShowList(const Caption : string;List: TListBox; FileNames: TStringList);
var
  i : Integer;
begin
  List.Items.BeginUpdate;
  try
    //List.Clear;
    List.Items.Add(Caption);
    for i := 0 to FileNames.Count-1 do begin
      List.Items.Add(ExtractFileName(FileNames[i]));
    end;

  finally
    List.Items.EndUpdate;
  end;
end;

procedure TFormMain.Button1Click(Sender: TObject);
var
  Dialog: TFileOpenDialog;
begin
  Dialog := TFileOpenDialog.Create(nil);
  try
    Dialog.Options := [fdoPickFolders];
    Dialog.DefaultFolder := Edit1.Text;
    if Dialog.Execute then
      Edit1.Text := Dialog.FileName;
      FFolderWatch.FolderPath := Edit1.Text;
  finally
    Dialog.Free;
  end;
end;

end.

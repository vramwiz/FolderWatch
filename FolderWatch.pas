unit FolderWatch;

{
  Unit Name  : FolderWatch

  �T�v:
    ���̃��j�b�g�͎w�肳�ꂽ�t�H���_���Ď����A�t�@�C���̒ǉ��E�폜�E�X�V��
    �����Ō��o���ăC�x���g�Œʒm����@�\��񋟂��܂��B

  ��ȃN���X:
    - TFolderWatch
        �t�H���_�Ď��̃��C���N���X�B�v���p�e�B��ʂ��ĊĎ��Ώۃt�H���_��
        �T�u�t�H���_�̊Ď��ہA�e��C�x���g�n���h����ݒ�\�ł��B

    - TFolderWatchThread
        �o�b�N�O���E���h�ŊĎ������s����X���b�h�N���X�B���Ԋu�Ńt�H���_���X�L�������A
        �O��̃X�i�b�v�V���b�g�Ɣ�r���č��������o���܂��B

  �C�x���g:
    - OnFileAdd:     �t�@�C�����ǉ����ꂽ�Ƃ��ɔ������܂��B
    - OnFileDelete:  �t�@�C�����폜���ꂽ�Ƃ��ɔ������܂��B
    - OnFileUpdate:  �����t�@�C�����X�V���ꂽ�Ƃ��ɔ������܂��B
    - OnFileChange:  ��L���ׂĂ̕ύX�i�ǉ��E�폜�E�X�V�j���܂Ƃ߂Ēʒm���܂��B

  ���ӓ_:
    - �X���b�h�Ńt�H���_�����X�L������������ł��邽�߁A
      �t�@�C���V�X�e���̒ʒmAPI�iFindFirstChangeNotification ���j�͎g�p���Ă��܂���B
    - �X�L�����Ԋu��ʒm�̗��x�͓��������Ɉˑ����܂��B
    - �X���b�h�̓o�b�N�O���E���h�Ŏ��s����܂����A�C�x���g�n���h���̏�����
      ���C���X���b�h�ł��邱�Ƃ͕ۏ؂���܂���i�K�v�ɉ����ē����������s���Ă��������j�B

  ���p��:
    var
      Watcher: TFolderWatch;
    begin
      Watcher := TFolderWatch.Create;
      Watcher.FolderPath := 'C:\MyFolder';
      Watcher.OnFileAdd := MyAddHandler;
      Watcher.Start;
    end;
}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls,System.Generics.Collections;


type  TFolderWatchEvent = procedure(Sender: TObject; const FileNames: TStringList) of object;
type  TFolderWatchAllEvent = procedure(Sender: TObject; const AddFiles: TStringList; const DelFiles: TStringList; const UpdateFiles: TStringList) of object;

type
  TFolderWatchThread = class(TThread)
  private
    FFolderPath        : string;
    FFirstScanDone     : Boolean;
    FIncludeSubFolders : Boolean;
    FNotifyFilter      : DWORD;
    FExtFilters        : TStringList;
    FOwner             : TObject; // �� �^�𒊏ۉ�

    FLastSnapshot: TDictionary<string, TDateTime>; // �Ď��L���b�V��
    FNotificationHandle: THandle;
    // �w��t�H���_���̑S�t�@�C���𑖍�
    procedure ScanAndCompare;
    // �t�@�C��������
    procedure ScanSearch(CurrentFiles: TDictionary<string, TDateTime>);
    // ���݂̏���ۑ�
    procedure ScanUpdate(CurrentFiles: TDictionary<string, TDateTime>);
    // �X�L�����𖳎�
    procedure ScanDone();
    // �t�@�C�����������Ɏ��AExtFilters ���Q�Ƃ��āA�\�����Ă悯��� True
    function IsVisible(const FileName: string): Boolean;
    //�X���b�h��������I�[�i�[�iTFolderWatch�j�ɑ΂��ăC�x���g���X���b�h�Z�[�t�ɒʒm���܂��B
    procedure NotifyChanges(const AddList, DelList, ModList: TStringList);
  protected
    procedure Execute; override;
  public
    constructor Create(AOwner: TObject);
    destructor Destroy; override;
  end;

type
  TFolderWatch = class(TPersistent )
  private
    { Private �錾 }
    FFolderPath        : string;
    FIncludeSubFolders : Boolean;
    FNotifyFilter      : DWORD;
    FThread            : TFolderWatchThread;
    FOnFileChange      : TFolderWatchAllEvent;
    FOnFileAdd         : TFolderWatchEvent;
    FOnFileDelete      : TFolderWatchEvent;
    FOnFileUpdate      : TFolderWatchEvent;
    FFirstScanDone     : Boolean;
    FExtFilters        : TStringList;
    procedure StartWatching;
    procedure StopWatching;
    procedure RestartWatchingIfRunning(const Action: TProc);
    procedure SetFolderPath(const Value: string);
    procedure SetIncludeSubFolders(const Value: Boolean);
    procedure SetNotifyFilter(const Value: DWORD);
    procedure SetFirstScanDone(const Value: Boolean);
  protected
    procedure DoFileAdd(const FileNames: TStringList);
    procedure DoFileDelete(const FileNames: TStringList);
    procedure DoFileUpdate(const FileNames: TStringList);
    procedure DoFileChange(const AddFiles: TStringList; const DelFiles: TStringList; const UpdateFiles: TStringList);
  public
    constructor Create();
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property FolderPath: string read FFolderPath write SetFolderPath;
    property IncludeSubFolders: Boolean read FIncludeSubFolders write SetIncludeSubFolders;
    property NotifyFilter: DWORD read FNotifyFilter write SetNotifyFilter;
    // ����̓C�x���g�𔭐������Ȃ�
    property FirstScanDone : Boolean read FFirstScanDone write SetFirstScanDone;
    // �g���q�t�B���^�[ ���ݒ��ċN�����K�v
    property ExtFilters: TStringList read FExtFilters;
    property OnFileChange: TFolderWatchAllEvent read FOnFileChange write FOnFileChange;
    property OnFileAdd: TFolderWatchEvent       read FOnFileAdd    write FOnFileAdd;
    property OnFileDelete: TFolderWatchEvent    read FOnFileDelete write FOnFileDelete;
    property OnFileUpdate: TFolderWatchEvent    read FOnFileUpdate write FOnFileUpdate;
  end;

implementation

uses System.IOUtils;

const
  MinTimeGapSec = 1.0; // 1�b�ȓ��̕ύX�͖���

{ TFolderWatch }

constructor TFolderWatch.Create;
begin
  inherited Create;

  // �����l�̐ݒ�i�K�v�ɉ����āj
  FFolderPath := '';
  FIncludeSubFolders := False;

  FExtFilters := TStringList.Create;
  FExtFilters.CaseSensitive := False;

  // �����o�^�i�K�v�Ȃ�O������ǉ��^�폜�\�j
  FExtFilters.Add('.crdownload');
  FExtFilters.Add('.tmp');
  FExtFilters.Add('.part');
  FNotifyFilter := FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_LAST_WRITE;
end;

destructor TFolderWatch.Destroy;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FThread.Free;
  end;

  inherited Destroy;
end;

procedure TFolderWatch.Start;
begin
  StartWatching;
end;

procedure TFolderWatch.StartWatching;
begin
  if Assigned(FThread) then
    Exit; // ���łɊJ�n����Ă���Ζ���

  FThread := TFolderWatchThread.Create(Self);
  FThread.FFolderPath := FFolderPath;
  FThread.FIncludeSubFolders := FIncludeSubFolders;
  FThread.FNotifyFilter := FNotifyFilter;
  FThread.FreeOnTerminate := False;
  FThread.FFirstScanDone := FFirstScanDone;
  FThread.FExtFilters.Assign(FExtFilters);
  FThread.Start;
end;

procedure TFolderWatch.Stop;
begin
  StopWatching;
end;

procedure TFolderWatch.StopWatching;
begin
  if Assigned(FThread) then
  begin
    FThread.Terminate;
    FThread.WaitFor;
    FreeAndNil(FThread);
  end;
end;

procedure TFolderWatch.SetFirstScanDone(const Value: Boolean);
begin
  FFirstScanDone := Value;
  if FFirstScanDone = Value then Exit;

  RestartWatchingIfRunning(
    procedure
    begin
      FFirstScanDone := Value;
    end);
end;

procedure TFolderWatch.SetFolderPath(const Value: string);
begin
  if SameText(FFolderPath, Value) then Exit;

  RestartWatchingIfRunning(
    procedure
    begin
      FFolderPath := Value;
    end);
end;

procedure TFolderWatch.SetIncludeSubFolders(const Value: Boolean);
begin
  if FIncludeSubFolders = Value then Exit;

  RestartWatchingIfRunning(
    procedure
    begin
      FIncludeSubFolders := Value;
    end);
end;

procedure TFolderWatch.SetNotifyFilter(const Value: DWORD);
begin
  if FNotifyFilter = Value then
    Exit;

  RestartWatchingIfRunning(
    procedure
    begin
      FNotifyFilter := Value;
    end);
end;

procedure TFolderWatch.RestartWatchingIfRunning(const Action: TProc);
var
  WasRunning: Boolean;
begin
  WasRunning := Assigned(FThread) and (FThread.Finished = False);
  if WasRunning then
    StopWatching;

  Action(); // �l�̕ύX

  if WasRunning then
    StartWatching;
end;


procedure TFolderWatch.DoFileAdd(const FileNames: TStringList);
begin
 if Assigned(FOnFileAdd) then FOnFileAdd(Self,FileNames);
end;

procedure TFolderWatch.DoFileUpdate(const FileNames: TStringList);
begin
 if Assigned(FOnFileUpdate) then FOnFileUpdate(Self,FileNames);
end;

procedure TFolderWatch.DoFileDelete(const FileNames: TStringList);
begin
 if Assigned(FOnFileDelete) then FOnFileDelete(Self,FileNames);
end;

procedure TFolderWatch.DoFileChange(const AddFiles, DelFiles,UpdateFiles: TStringList);
begin
 if Assigned(FOnFileChange) then FOnFileChange(Self,AddFiles, DelFiles,UpdateFiles);
end;


{ TFolderWatchThread }

constructor TFolderWatchThread.Create(AOwner: TObject);
begin
  inherited Create(True); // Suspended=True�iStart�͊O�ŌĂԁj

  FreeOnTerminate := False; // �����I�ɉ������z��
  FOwner := AOwner;

  FFolderPath := '';
  FIncludeSubFolders := False;
  FNotifyFilter := FILE_NOTIFY_CHANGE_FILE_NAME or
                   FILE_NOTIFY_CHANGE_LAST_WRITE or
                   FILE_NOTIFY_CHANGE_SIZE;

  FExtFilters := TStringList.Create;
  FNotificationHandle := 0;
  FLastSnapshot := TDictionary<string, TDateTime>.Create;
end;

destructor TFolderWatchThread.Destroy;
begin
  if FNotificationHandle <> 0 then
  begin
    FindCloseChangeNotification(FNotificationHandle);
    FNotificationHandle := 0;
  end;

  FLastSnapshot.Free;
  FExtFilters.Free;
  inherited;
end;

procedure TFolderWatchThread.Execute;
begin
  FNotificationHandle := FindFirstChangeNotification(
    PChar(FFolderPath),
    FIncludeSubFolders,
    FNotifyFilter
  );

  if FNotificationHandle = INVALID_HANDLE_VALUE then
    Exit;

  if FFirstScanDone then begin
    ScanDone();
  end;

  try
    // ����X�i�b�v�V���b�g�쐬
    ScanAndCompare;

    while not Terminated do
    begin
      // �ύX���ʒm�����܂őҋ@
      case WaitForSingleObject(FNotificationHandle, 500) of
        WAIT_OBJECT_0:
        begin
          if Terminated then Break;
          Sleep(500);
          ScanAndCompare;

          // ���̒ʒm���Đݒ�
          if not FindNextChangeNotification(FNotificationHandle) then
            Break;
        end;

        WAIT_FAILED:
          Break;
      end;
    end;

  finally
    if FNotificationHandle <> 0 then
    begin
      FindCloseChangeNotification(FNotificationHandle);
      FNotificationHandle := 0;
    end;
  end;
end;

function TFolderWatchThread.IsVisible(const FileName: string): Boolean;
var
  Ext: string;
begin
  Ext := LowerCase(ExtractFileExt(FileName));
  Result := FExtFilters.IndexOf(Ext) = -1; // �t�B���^�[�Ɋ܂܂�Ă��Ȃ���Ε\���Ώ�
end;

procedure TFolderWatchThread.NotifyChanges(const AddList, DelList,
  ModList: TStringList);
var
  Owner: TFolderWatch;
begin
  if not (FOwner is TFolderWatch) then
    Exit;

  Owner := TFolderWatch(FOwner);

  // �ꊇ�ʒm�iOnFileChange�j
  Synchronize(procedure begin
    Owner.DoFileChange(AddList, DelList, ModList);
  end);

  // �ʒʒm�iDoFileAdd�j
  if (AddList.Count > 0) then begin
    Synchronize(procedure begin
      Owner.DoFileAdd(AddList);
    end);
  end;

  if (DelList.Count > 0) then begin
    Synchronize(procedure begin
      Owner.DoFileDelete(DelList);
    end);
  end;

  if (ModList.Count > 0) then begin
    Synchronize(procedure begin
      Owner.DoFileUpdate(ModList);
    end);
  end;
end;

procedure TFolderWatchThread.ScanAndCompare;
var
  CurrentFiles: TDictionary<string, TDateTime>;
  FilePath: string;
  AddList, DelList, ModList: TStringList;
  Pair: TPair<string, TDateTime>;
  Value: TDateTime;
begin
  CurrentFiles := TDictionary<string, TDateTime>.Create;
  AddList := TStringList.Create;
  DelList := TStringList.Create;
  ModList := TStringList.Create;
  try
    // �ċA���Ȃ��P���ȑ����i�ċA�Ή��͂��Ƃŉ��ǉj
    ScanSearch(CurrentFiles);

    // �ǉ��^�X�V�`�F�b�N
    for Pair in CurrentFiles do
    begin
      if not FLastSnapshot.TryGetValue(Pair.Key, Value) then
      begin
        AddList.Add(Pair.Key); // �V�K�ǉ�
      end
      else if (FLastSnapshot[Pair.Key] <> Pair.Value) and
              ((Now - FLastSnapshot[Pair.Key]) * 86400 > MinTimeGapSec) then
      begin
        ModList.Add(Pair.Key); // �X�V����
      end;
    end;

    // �폜�`�F�b�N
    for Pair in FLastSnapshot do
    begin
      if not CurrentFiles.ContainsKey(Pair.Key) then
        DelList.Add(Pair.Key);
    end;

    // �ʒm���s
    NotifyChanges(AddList, DelList, ModList);

    // ���݂̏���ۑ�
    ScanUpdate(CurrentFiles);

  finally
    AddList.Free;
    DelList.Free;
    ModList.Free;
    CurrentFiles.Free;
  end;
end;

procedure TFolderWatchThread.ScanSearch(
  CurrentFiles: TDictionary<string, TDateTime>);
var
  SearchRec: TSearchRec;
  FilePath: string;
begin
  // �ċA���Ȃ��P���ȑ����i�ċA�Ή��͂��Ƃŉ��ǉj
  if FindFirst(IncludeTrailingPathDelimiter(FFolderPath) + '*.*', faAnyFile, SearchRec) = 0 then
  begin
    repeat
      if (SearchRec.Attr and faDirectory) = 0 then
      begin
        if not IsVisible(SearchRec.Name) then Continue;   // �Ď��Ώۂ����f
        FilePath := IncludeTrailingPathDelimiter(FFolderPath) + SearchRec.Name;
        CurrentFiles.Add(FilePath, SearchRec.TimeStamp);  // Delphi10�ȍ~�Ή�
      end;
    until FindNext(SearchRec) <> 0;
    FindClose(SearchRec);
  end;
end;

procedure TFolderWatchThread.ScanUpdate(
  CurrentFiles: TDictionary<string, TDateTime>);
var
  Pair: TPair<string, TDateTime>;
begin
  FLastSnapshot.Clear;
  for Pair in CurrentFiles do begin
    if not IsVisible(Pair.Key) then Continue;   // �Ď��Ώۂ����f
    FLastSnapshot.Add(Pair.Key, Pair.Value);
  end;
end;

procedure TFolderWatchThread.ScanDone;
var
  CurrentFiles: TDictionary<string, TDateTime>;
begin
  CurrentFiles := TDictionary<string, TDateTime>.Create;
  try
    ScanSearch(CurrentFiles);
    ScanUpdate(CurrentFiles);
  finally
    CurrentFiles.Free;
  end;

end;


end.

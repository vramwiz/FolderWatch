# TFolderWatchThread

`TFolderWatchThread` は、指定したフォルダを監視し、ファイルの追加・削除・変更をリアルタイムで検知する Delphi 用スレッドクラスです。

## 特徴

- `FindFirstChangeNotification` API を使用して高効率なファイル監視を実現
- ファイルの追加／削除／更新を `TStringList` で分類して通知
- 最初のスキャンを通知対象に含めるかスキップするかを `FFirstScanDone` により制御可能
- 再帰サブフォルダ監視（`FIncludeSubFolders`）に対応予定（現在は未実装）

## 主なプロパティ

| プロパティ名         | 型                      | 説明 |
|----------------------|--------------------------|------|
| `FFolderPath`        | `string`                 | 監視対象のフォルダパス（必須） |
| `FNotifyFilter`      | `DWORD`                  | 監視する変更内容（`FILE_NOTIFY_CHANGE_*` の組み合わせ） |
| `FIncludeSubFolders` | `Boolean`                | サブフォルダを含めて監視するか（※現在未使用） |
| `FFirstScanDone`     | `Boolean`                | 起動時に初回スキャンを無視するかどうか（True = 無視） |

## イベントハンドラ

```pascal
procedure OnFileChanged(Sender: TObject;  const AddList, DelList, ModList: TStringList) of object;
```

- 追加・削除・更新されたファイルパス一覧を通知します。

## 使用例

```pascal
FolderWatch := TFolderWatchThread.Create;
FolderWatch.FolderPath := 'C:\MyFolder';
FolderWatch.OnFileChange := YourChangeHandler;
FolderWatch.FFirstScanDone := True; // 初回は無視する
FolderWatch.Start;
```

## 拡張予定

- サブフォルダの再帰監視
- マルチイベント通知対応（ファイル名とタイムスタンプ両方の変化通知）

## ライセンス

MIT License

# Delphiで作るフォルダ監視クラス「TFolderWatch」

## 概要

このライブラリは、Delphi環境で特定フォルダの変更を監視するためのクラス `TFolderWatch` を提供します。  
ファイルの追加・更新・削除などをイベントで通知でき、サブフォルダの監視や通知フィルタの設定も可能です。

## 特徴

- `FindFirstChangeNotification` APIを利用した軽量な監視
- フォルダ内ファイルのスナップショット比較による変更検出
- サブフォルダの再帰監視対応（オプション）
- `FILE_NOTIFY_CHANGE_*` ビットフラグで柔軟な監視フィルタ設定
- 初回スキャンは通知せずスナップショットの保存のみ（設定可能）

## 使用方法

```pascal
var
  Watcher: TFolderWatch;
begin
  Watcher := TFolderWatch.Create;
  Watcher.FolderPath := 'C:\MyFolder';
  Watcher.IncludeSubFolders := True;
  Watcher.NotifyFilter := FILE_NOTIFY_CHANGE_FILE_NAME or FILE_NOTIFY_CHANGE_LAST_WRITE;
  Watcher.OnChange := procedure(Sender: TObject; const AddList, DelList, ModList: TStringList)
    begin
      // ここで変更を処理する
    end;
  Watcher.StartWatching;
end;
```

## インストール

このクラスは単一ユニットで構成されており、他の依存ライブラリは不要です。  
ユニットファイルをプロジェクトに追加するだけで使用可能です。

## イベント

- `OnChange`: ファイルの追加・削除・更新を個別のリストで通知

## 注意点

- `StartWatching` を呼び出すまでは監視は開始されません
- `Stop` を呼び出すことでスレッドを停止・解放します
- 初回の変更通知を無視したい場合は `FirstScanDone := True` を設定してください

## 対応環境

- Delphi 10 以降推奨（`TDateTime` の `SearchRec.TimeStamp` を使用）

## ライセンス

MIT License

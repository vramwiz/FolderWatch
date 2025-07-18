object FormMain: TFormMain
  Left = 0
  Top = 0
  Caption = 'FormMain'
  ClientHeight = 231
  ClientWidth = 505
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 0
    Top = 25
    Width = 121
    Height = 206
    Align = alLeft
    ItemHeight = 13
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 505
    Height = 25
    Align = alTop
    TabOrder = 1
    object Edit1: TEdit
      Left = 1
      Top = 1
      Width = 463
      Height = 23
      Align = alClient
      ReadOnly = True
      TabOrder = 0
      ExplicitHeight = 21
    end
    object Button1: TButton
      Left = 464
      Top = 1
      Width = 40
      Height = 23
      Align = alRight
      Caption = '..'
      TabOrder = 1
      OnClick = Button1Click
    end
  end
  object ListBox2: TListBox
    Left = 121
    Top = 25
    Width = 121
    Height = 206
    Align = alLeft
    ItemHeight = 13
    TabOrder = 2
  end
  object ListBox3: TListBox
    Left = 242
    Top = 25
    Width = 121
    Height = 206
    Align = alLeft
    ItemHeight = 13
    TabOrder = 3
  end
end

object FormConfig: TFormConfig
  Left = 529
  Top = 544
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsToolWindow
  Caption = 'Lib Simulator'
  ClientHeight = 333
  ClientWidth = 489
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  Icon.Data = {
    0000010001001010100000000000280100001600000028000000100000002000
    00000100040000000000C0000000000000000000000000000000000000000000
    000000008000008000000080800080000000800080008080000080808000C0C0
    C0000000FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    0000000000000000000000000000000000000000000000000000000000000000
    000000000000000000000000000000000000000000000000000000000000FFFF
    0000FFFF0000FFFF0000FFFF00009EDE00006EDE0000EEDE0000DEDE0000BEDE
    00007ED200006ECC00009EDE0000FFFF0000FFFF0000FFFF0000FFFF0000}
  OldCreateOrder = False
  Position = poScreenCenter
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GB_Error: TGroupBox
    Left = 8
    Top = 276
    Width = 321
    Height = 49
    Caption = ' Vyvolat chybu '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 16
      Width = 45
      Height = 13
      Caption = 'ID chyby:'
    end
    object Label2: TLabel
      Left = 135
      Top = 16
      Width = 58
      Height = 13
      Caption = 'MTB deska:'
    end
    object SE_Err_id: TSpinEdit
      Left = 64
      Top = 16
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 0
      Value = 0
      OnKeyPress = SE_Err_boardKeyPress
    end
    object SE_Err_board: TSpinEdit
      Left = 199
      Top = 16
      Width = 65
      Height = 22
      MaxValue = 0
      MinValue = 0
      TabOrder = 1
      Value = 255
      OnKeyPress = SE_Err_boardKeyPress
    end
    object B_Error: TButton
      Left = 270
      Top = 16
      Width = 43
      Height = 21
      Caption = 'OK'
      TabOrder = 2
      OnClick = B_ErrorClick
    end
  end
end

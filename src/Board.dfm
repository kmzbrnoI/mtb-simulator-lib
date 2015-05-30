object F_Board: TF_Board
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Editovat desku [adresa]'
  ClientHeight = 225
  ClientWidth = 233
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 38
    Height = 13
    Caption = 'Adresa:'
  end
  object Label2: TLabel
    Left = 8
    Top = 40
    Width = 34
    Height = 13
    Caption = 'N'#225'zev:'
  end
  object Label3: TLabel
    Left = 8
    Top = 70
    Width = 22
    Height = 13
    Caption = 'Typ:'
  end
  object L_adresa: TLabel
    Left = 195
    Top = 8
    Width = 30
    Height = 13
    Alignment = taRightJustify
    Caption = '[addr]'
  end
  object Label4: TLabel
    Left = 8
    Top = 97
    Width = 20
    Height = 13
    Caption = 'FW:'
  end
  object CB_Type: TComboBox
    Left = 80
    Top = 70
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    Items.Strings = (
      'MTB-UNI'
      'MTB-UNI out'
      'MTB-TTL'
      'MTB-TTL out'
      'MTB-REG puls'
      'MTB-POT')
  end
  object E_Name: TEdit
    Left = 80
    Top = 40
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object B_Apply: TButton
    Left = 150
    Top = 194
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    TabOrder = 2
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 69
    Top = 194
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 3
    OnClick = B_StornoClick
  end
  object E_FW: TEdit
    Left = 80
    Top = 97
    Width = 145
    Height = 21
    TabOrder = 4
  end
  object RG_Exists: TRadioGroup
    Left = 8
    Top = 124
    Width = 217
    Height = 64
    Caption = ' Existence '
    Items.Strings = (
      'Ne'
      'Ano')
    TabOrder = 5
  end
end

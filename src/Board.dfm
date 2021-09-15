object F_Board: TF_Board
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Editovat desku [adresa]'
  ClientHeight = 393
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
  object E_Name: TEdit
    Left = 80
    Top = 43
    Width = 145
    Height = 21
    TabOrder = 0
  end
  object B_Apply: TButton
    Left = 150
    Top = 359
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    TabOrder = 2
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 69
    Top = 359
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
  object RG_Failure: TRadioGroup
    Left = 8
    Top = 194
    Width = 217
    Height = 64
    Caption = ' Porucha '
    Items.Strings = (
      'Ne'
      'Ano')
    TabOrder = 6
  end
  object GB_IO_type: TGroupBox
    Left = 8
    Top = 262
    Width = 217
    Height = 91
    Caption = ' Typ vstup'#367' a v'#253'stup'#367' '
    TabOrder = 7
    object chb_IR0: TCheckBox
      Left = 16
      Top = 16
      Width = 65
      Height = 17
      Caption = 'IR 0-3'
      TabOrder = 0
    end
    object chb_IR3: TCheckBox
      Left = 16
      Top = 64
      Width = 65
      Height = 17
      Caption = 'IR 12-15'
      TabOrder = 1
    end
    object chb_IR2: TCheckBox
      Left = 16
      Top = 48
      Width = 65
      Height = 17
      Caption = 'IR 8-11'
      TabOrder = 2
    end
    object chb_IR1: TCheckBox
      Left = 16
      Top = 32
      Width = 65
      Height = 17
      Caption = 'IR 4-7'
      TabOrder = 3
    end
    object chb_SCOM0: TCheckBox
      Left = 113
      Top = 16
      Width = 75
      Height = 17
      Caption = 'S-COM 0,1'
      TabOrder = 4
    end
    object chb_SCOM1: TCheckBox
      Left = 113
      Top = 32
      Width = 75
      Height = 17
      Caption = 'S-COM 2,3'
      TabOrder = 5
    end
    object chb_SCOM2: TCheckBox
      Left = 113
      Top = 48
      Width = 75
      Height = 17
      Caption = 'S-COM 4,5'
      TabOrder = 6
    end
    object chb_SCOM3: TCheckBox
      Left = 113
      Top = 64
      Width = 75
      Height = 17
      Caption = 'S-COM 6,7'
      TabOrder = 7
    end
  end
  object E_Type: TEdit
    Left = 80
    Top = 70
    Width = 145
    Height = 21
    TabOrder = 1
  end
end

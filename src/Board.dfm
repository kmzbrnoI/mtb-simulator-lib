object F_Board: TF_Board
  Left = 0
  Top = 0
  BorderStyle = bsToolWindow
  Caption = 'Editovat desku [adresa]'
  ClientHeight = 321
  ClientWidth = 418
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
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
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Pou'#382#237't'
    Default = True
    TabOrder = 8
    OnClick = B_ApplyClick
  end
  object B_Storno: TButton
    Left = 69
    Top = 288
    Width = 75
    Height = 25
    Caption = 'Storno'
    TabOrder = 9
    OnClick = B_StornoClick
  end
  object E_FW: TEdit
    Left = 80
    Top = 97
    Width = 145
    Height = 21
    TabOrder = 2
  end
  object GB_IO_type: TGroupBox
    Left = 231
    Top = 8
    Width = 178
    Height = 305
    Caption = ' Typy vstup'#367' a v'#253'stup'#367' '
    TabOrder = 7
    object CHB_IR0: TCheckBox
      Left = 16
      Top = 16
      Width = 41
      Height = 17
      Caption = 'IR 0'
      TabOrder = 0
    end
    object CHB_IR3: TCheckBox
      Tag = 3
      Left = 16
      Top = 64
      Width = 41
      Height = 17
      Caption = 'IR 3'
      TabOrder = 3
    end
    object CHB_IR2: TCheckBox
      Tag = 2
      Left = 16
      Top = 48
      Width = 41
      Height = 17
      Caption = 'IR 2'
      TabOrder = 2
    end
    object CHB_IR1: TCheckBox
      Tag = 1
      Left = 16
      Top = 32
      Width = 41
      Height = 17
      Caption = 'IR 1'
      TabOrder = 1
    end
    object CHB_IR6: TCheckBox
      Tag = 6
      Left = 16
      Top = 119
      Width = 41
      Height = 17
      Caption = 'IR 6'
      TabOrder = 6
    end
    object CHB_IR5: TCheckBox
      Tag = 5
      Left = 16
      Top = 103
      Width = 41
      Height = 17
      Caption = 'IR 5'
      TabOrder = 5
    end
    object CHB_IR4: TCheckBox
      Tag = 4
      Left = 16
      Top = 87
      Width = 41
      Height = 17
      Caption = 'IR 4'
      TabOrder = 4
    end
    object CHB_IR7: TCheckBox
      Tag = 7
      Left = 16
      Top = 135
      Width = 41
      Height = 17
      Caption = 'IR 7'
      TabOrder = 7
    end
    object CHB_IR15: TCheckBox
      Tag = 15
      Left = 16
      Top = 277
      Width = 41
      Height = 17
      Caption = 'IR 15'
      TabOrder = 15
    end
    object CHB_IR14: TCheckBox
      Tag = 14
      Left = 16
      Top = 261
      Width = 41
      Height = 17
      Caption = 'IR 14'
      TabOrder = 14
    end
    object CHB_IR13: TCheckBox
      Tag = 13
      Left = 16
      Top = 245
      Width = 41
      Height = 17
      Caption = 'IR 13'
      TabOrder = 13
    end
    object CHB_IR12: TCheckBox
      Tag = 12
      Left = 16
      Top = 229
      Width = 41
      Height = 17
      Caption = 'IR 12'
      TabOrder = 12
    end
    object CHB_IR11: TCheckBox
      Tag = 11
      Left = 16
      Top = 206
      Width = 41
      Height = 17
      Caption = 'IR 11'
      TabOrder = 11
    end
    object CHB_IR10: TCheckBox
      Tag = 10
      Left = 16
      Top = 190
      Width = 41
      Height = 17
      Caption = 'IR 10'
      TabOrder = 10
    end
    object CHB_IR9: TCheckBox
      Tag = 9
      Left = 16
      Top = 174
      Width = 41
      Height = 17
      Caption = 'IR 9'
      TabOrder = 9
    end
    object CHB_IR8: TCheckBox
      Tag = 8
      Left = 16
      Top = 158
      Width = 41
      Height = 17
      Caption = 'IR 8'
      TabOrder = 8
    end
    object CHB_SCOM0: TCheckBox
      Left = 96
      Top = 16
      Width = 65
      Height = 17
      Caption = 'S-COM 0'
      TabOrder = 16
    end
    object CHB_SCOM1: TCheckBox
      Tag = 1
      Left = 96
      Top = 32
      Width = 65
      Height = 17
      Caption = 'S-COM 1'
      TabOrder = 17
    end
    object CHB_SCOM2: TCheckBox
      Tag = 2
      Left = 96
      Top = 48
      Width = 65
      Height = 17
      Caption = 'S-COM 2'
      TabOrder = 18
    end
    object CHB_SCOM3: TCheckBox
      Tag = 3
      Left = 96
      Top = 64
      Width = 65
      Height = 17
      Caption = 'S-COM 3'
      TabOrder = 19
    end
    object CHB_SCOM4: TCheckBox
      Tag = 4
      Left = 96
      Top = 87
      Width = 65
      Height = 17
      Caption = 'S-COM 4'
      TabOrder = 20
    end
    object CHB_SCOM5: TCheckBox
      Tag = 5
      Left = 96
      Top = 103
      Width = 65
      Height = 17
      Caption = 'S-COM 5'
      TabOrder = 21
    end
    object CHB_SCOM7: TCheckBox
      Tag = 7
      Left = 96
      Top = 135
      Width = 65
      Height = 17
      Caption = 'S-COM 7'
      TabOrder = 23
    end
    object CHB_SCOM6: TCheckBox
      Tag = 6
      Left = 96
      Top = 119
      Width = 65
      Height = 17
      Caption = 'S-COM 6'
      TabOrder = 22
    end
    object CHB_SCOM8: TCheckBox
      Tag = 8
      Left = 96
      Top = 158
      Width = 65
      Height = 17
      Caption = 'S-COM 8'
      TabOrder = 24
    end
    object CHB_SCOM9: TCheckBox
      Tag = 9
      Left = 96
      Top = 174
      Width = 65
      Height = 17
      Caption = 'S-COM 9'
      TabOrder = 25
    end
    object CHB_SCOM10: TCheckBox
      Tag = 10
      Left = 96
      Top = 190
      Width = 65
      Height = 17
      Caption = 'S-COM 10'
      TabOrder = 26
    end
    object CHB_SCOM11: TCheckBox
      Tag = 11
      Left = 96
      Top = 206
      Width = 65
      Height = 17
      Caption = 'S-COM 11'
      TabOrder = 27
    end
    object CHB_SCOM12: TCheckBox
      Tag = 12
      Left = 96
      Top = 229
      Width = 65
      Height = 17
      Caption = 'S-COM 12'
      TabOrder = 28
    end
    object CHB_SCOM13: TCheckBox
      Tag = 13
      Left = 96
      Top = 245
      Width = 65
      Height = 17
      Caption = 'S-COM 13'
      TabOrder = 29
    end
    object CHB_SCOM14: TCheckBox
      Tag = 14
      Left = 96
      Top = 261
      Width = 65
      Height = 17
      Caption = 'S-COM 14'
      TabOrder = 30
    end
    object CHB_SCOM15: TCheckBox
      Tag = 15
      Left = 96
      Top = 277
      Width = 65
      Height = 17
      Caption = 'S-COM 15'
      TabOrder = 31
    end
  end
  object E_Type: TEdit
    Left = 80
    Top = 70
    Width = 145
    Height = 21
    TabOrder = 1
  end
  object CHB_Exists: TCheckBox
    Left = 8
    Top = 124
    Width = 193
    Height = 17
    Caption = 'Existence (pozor: nem'#283'nit za b'#283'hu)'
    TabOrder = 3
  end
  object CHB_failure: TCheckBox
    Left = 8
    Top = 147
    Width = 177
    Height = 17
    Caption = 'Failure (nep'#345#237'tomen na sb'#283'rnici)'
    TabOrder = 4
  end
  object CHB_Error: TCheckBox
    Left = 8
    Top = 170
    Width = 136
    Height = 17
    Caption = 'Error (modul hl'#225's'#237' chybu)'
    TabOrder = 5
  end
  object CHB_Warning: TCheckBox
    Left = 8
    Top = 193
    Width = 177
    Height = 17
    Caption = 'Warning (modul hl'#225's'#237' varov'#225'n'#237')'
    TabOrder = 6
  end
end

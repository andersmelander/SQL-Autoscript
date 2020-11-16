object DataModuleAutoScriptRunnerFireDAC: TDataModuleAutoScriptRunnerFireDAC
  OldCreateOrder = False
  Height = 338
  Width = 215
  object FDScript: TFDScript
    SQLScripts = <
      item
      end>
    Connection = FDConnection
    ScriptOptions.EchoCommands = ecNone
    ScriptOptions.FeedbackCommands = False
    ScriptOptions.FeedbackScript = False
    ScriptOptions.SpoolOutput = smOnReset
    ScriptOptions.CommandSeparator = ';'
    Params = <>
    Macros = <>
    Left = 124
    Top = 20
  end
  object FDConnection: TFDConnection
    LoginPrompt = False
    Left = 40
    Top = 16
  end
  object QueryPatchLevel: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      'select'
      '  NUMBER,'
      '  MILESTONE'
      'from'
      '  PATCH_LEVEL')
    Left = 40
    Top = 76
  end
  object QueryAddPatchLevel: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      
        'insert into PATCH_LEVEL (NUMBER, DESCRIPTION, MILESTONE) VALUES ' +
        '(:ParamNumber, :ParamDescription, :ParamMilestone)')
    Left = 40
    Top = 128
    ParamData = <
      item
        Name = 'PARAMNUMBER'
        ParamType = ptInput
      end
      item
        Name = 'PARAMDESCRIPTION'
        ParamType = ptInput
      end
      item
        Name = 'PARAMMILESTONE'
        ParamType = ptInput
      end>
  end
end

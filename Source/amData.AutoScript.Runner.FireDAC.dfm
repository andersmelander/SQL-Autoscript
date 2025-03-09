object DataModuleAutoScriptRunnerFireDAC: TDataModuleAutoScriptRunnerFireDAC
  Height = 338
  Width = 215
  object FDScript: TFDScript
    SQLScripts = <
      item
      end>
    Connection = FDConnection
    Transaction = FDTransaction
    ScriptOptions.CommitEachNCommands = -1
    ScriptOptions.EchoCommands = ecAll
    ScriptOptions.SpoolOutput = smOnReset
    ScriptOptions.BreakOnError = True
    ScriptOptions.CommandSeparator = ';'
    ScriptOptions.RaisePLSQLErrors = True
    Params = <>
    Macros = <>
    OnProgress = FDScriptProgress
    OnError = FDScriptError
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
    Top = 132
  end
  object QueryAddPatchLevel: TFDQuery
    Connection = FDConnection
    SQL.Strings = (
      
        'insert into PATCH_LEVEL (NUMBER, DESCRIPTION, MILESTONE) VALUES ' +
        '(:ParamNumber, :ParamDescription, :ParamMilestone)')
    Left = 40
    Top = 184
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
  object FDTransaction: TFDTransaction
    Options.AutoStart = False
    Options.AutoStop = False
    Options.DisconnectAction = xdRollback
    Connection = FDConnection
    Left = 40
    Top = 68
  end
end

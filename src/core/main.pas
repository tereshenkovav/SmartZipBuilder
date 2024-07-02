unit main ;

interface
uses SysUtils, Generics.Collections, Classes,
  sevenzip ;

type
  TMain = class
  private
    compressionlevel:Cardinal ;
    compressionmethod:TZipCompressionMethod ;
    zipfile:string ;
    outpath:string ;
    texts:TDictionary<string,string> ;
    params:TDictionary<string,string> ;
    function SetMethodByStr(str:string):Boolean ;
    function SetLevelByStr(str:string):Boolean;
    function ParseCommand(const s:string; var cmd:string;
      var args:TArray<string>):Boolean ;
    procedure ExitWithError(const msg:string; code:Integer = 1) ;
    function formatStringSyms(const s:string):string ;
    function expandEnvVars(const str: string): string;
    function expandParams(const str: string): string;
  public
    procedure Run() ;
  end;

implementation
uses Windows ;

procedure TMain.ExitWithError(const msg: string; code: Integer);
begin
  Writeln(msg) ;
  Halt(code) ;
end;

function TMain.formatStringSyms(const s: string): string;
begin
  Result:=s.Replace('\n',#10).Replace('\r',#13) ;
end;

function TMain.expandEnvVars(const str: string): string;
var BufSize: Integer;
begin
  BufSize := ExpandEnvironmentStrings(PChar(str), nil, 0);
  if BufSize>0 then begin
    SetLength(Result, BufSize - 1);
    ExpandEnvironmentStrings(PChar(str),PChar(Result), BufSize);
  end
  else
    Result := '';
end;

function TMain.expandParams(const str: string): string;
var name:string ;
begin
  Result:=str ;
  for name in params.Keys do
    Result:=Result.Replace('$'+name+'$',params[name]) ;
end;

function TMain.ParseCommand(const s: string; var cmd: string;
  var args: TArray<string>): Boolean;
var arr:TArray<string> ;
    i:Integer ;
begin
  // В текущей реализации не поддерживается парсинг с кавычками - пробелы в именах нельзя
  // Нужно либо сделать чистовой паросер, либо убирать кавычки при возрате строки Split
  arr:=s.Split([' '],'"') ;
  if Length(arr)<2 then ExitWithError('Not found arguments: '+s) ;
  cmd:=arr[0] ;
  SetLength(args,Length(arr)-1) ;
  for i := 0 to Length(arr)-2 do
   args[i]:=arr[i+1] ;
  Result:=True ;
end;

procedure TMain.Run() ;
var arc: I7zOutArchive;
    script:TStringList ;
    stms:TStringStream ;
    s,cmd,resfile,resdir,srcdir,textfile:string ;
    args:TArray<string> ;
    i,p:Integer ;
begin
  try
    Writeln('This source is prototype for development purposes only!!!') ;
    if ParamCount<1 then ExitWithError('Usage: scriptfile [arguments]') ;

    params:=TDictionary<string,string>.Create ;

    for i := 2 to ParamCount do begin
      if ParamStr(i)[1]<>'/' then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value') ;
      p:=ParamStr(i).IndexOf('=') ;
      if p=-1 then ExitWithError('Unknown argument: '+ParamStr(i)+', use /name=value') ;
      params.Add(ParamStr(i).Substring(1,p-1),ParamStr(i).Substring(p+1)) ;
    end;

    texts:=TDictionary<string,string>.Create() ;

    compressionlevel:=5 ;
    compressionmethod:=TZipCompressionMethod.mzDeflate ;
    zipfile:='' ;
    outpath:='.\' ;

    arc := CreateOutArchive(CLSID_CFormatZip);

    if not FileExists(ParamStr(1)) then
      ExitWithError('Not found script file: '+ParamStr(1)) ;

    script:=TStringList.Create() ;
    script.LoadFromFile(ParamStr(1)) ;
    for s in script do begin
      if s.Trim().Length=0 then Continue ;
      if s.Trim().StartsWith('#') then Continue ;
      if not ParseCommand(expandParams(expandEnvVars(s)),cmd,args) then
        ExitWithError('Error parsing line: '+s) ;
      cmd:=cmd.ToLower() ;
      if cmd='setcompressor' then begin
        if not SetMethodByStr(args[0]) then
          ExitWithError('Unknown compression method: '+args[0]) ;
      end;
      if cmd='setcompressionlevel' then begin
        if not SetLevelByStr(args[0]) then
          ExitWithError('Unknown compression level: '+args[0]) ;
      end;
      if cmd='zipfile' then begin
        zipfile:=args[0] ;
        Writeln('Setting zipfile: '+zipfile) ;
      end;
      if cmd='setoutpath' then begin
        outpath:=args[0].Trim() ;
        if outpath[length(outpath)]<>'\' then outpath:=outpath+'\' ;
        Writeln('Setting outpath: '+outpath) ;
      end;
      if cmd='files' then begin
        srcdir:=ExtractFilePath(args[0]) ;
        if srcdir='' then srcdir:='.' ;
        arc.AddFiles(srcdir, outpath, ExtractFileName(args[0]), false);
        Writeln('Adding files: '+args[0]) ;
      end;
      if cmd='dir' then begin
        if not DirectoryExists(args[0]) then ExitWithError('Not found directory: '+args[0]) ;
        if Length(args)>1 then resdir:=args[1] else resdir:=ExtractFileName(args[0]) ;
        arc.AddFiles(args[0], outpath+resdir, '*.*', True);
        Writeln('Adding dir recurse: '+args[0]+' as '+resdir) ;
      end;
      if cmd='file' then begin
        if not FileExists(args[0]) then ExitWithError('Not found file: '+args[0]) ;
        if Length(args)>1 then resfile:=args[1] else resfile:=ExtractFileName(args[0]) ;
        arc.AddFile(args[0], outpath+resfile) ;
        Writeln('Adding file: '+args[0]+' as '+resfile) ;
      end;
      if cmd='text' then begin
        if Length(args)<2 then ExitWithError('Command "Text" required two arguments: text and filename in archive') ;
        textfile:=outpath+args[1] ;
        if texts.ContainsKey(textfile) then begin
          texts[textfile]:=texts[textfile]+args[0] ;
          Writeln('Append string: '+args[0]+' to '+args[1]) ;
        end
        else begin
          texts.Add(textfile,args[0]) ;
          Writeln('Adding string: '+args[0]+' as '+args[1]) ;
        end;
      end;
    end;

    if zipfile='' then ExitWithError('Not set ZipFile') ;

    SetCompressionLevel(arc, compressionlevel);
    if compressionlevel>0 then SetCompressionMethod(arc, compressionmethod) ;

    for textfile in texts.Keys do begin
      stms:=TStringStream.Create(formatStringSyms(texts[textfile])) ;
      arc.AddStream(stms, soOwned, faArchive, CurrentFileTime, CurrentFileTime,
        textfile, false, false);
    end;

    if FileExists(zipfile) then SysUtils.DeleteFile(zipfile) ;

    arc.SaveToFile(zipfile);
    Writeln('Archive build OK') ;

    texts.Free ;
    script.Free ;
  except
    on E: Exception do
      Writeln('Error '+E.ClassName+': '+E.Message);
  end;
end ;

function TMain.SetMethodByStr(str: string):Boolean;
begin
  Result:=True ;
  str:=str.ToLower() ;
  if str='deflate' then compressionmethod:=TZipCompressionMethod.mzDeflate else
  if str='deflate64' then compressionmethod:=TZipCompressionMethod.mzDeflate64 else
  if str='bzip2' then compressionmethod:=TZipCompressionMethod.mzBZip2 else
  if str='lzma' then compressionmethod:=TZipCompressionMethod.mzLZMA else
  if str='ppmd' then compressionmethod:=TZipCompressionMethod.mzPPMD else
  Result:=False ;
end;

function TMain.SetLevelByStr(str: string):Boolean;
var n,code:Integer ;
begin
  Result:=True ;
  Val(str.Trim(),n,code) ;
  if code<>0 then Exit(False) ;
  if (n<0) or (n>9) then Exit(False) ;
  compressionlevel:=n ;
end;

end.

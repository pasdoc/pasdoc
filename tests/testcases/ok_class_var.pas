{ See bug [https://sourceforge.net/tracker/?func=detail&atid=104213&aid=1489442&group_id=4213] }

unit ok_class_var;

interface

type
  TPluginManager = class
  private
    FLoadedPlugins : TPluginCollection;
    FAfterPluginLoad : TAfterPluginLoadEvent;
    class var FInstance : TPluginManager;
  public
    LoadedPlugins : TPluginCollection;
    AfterPluginLoad : TAfterPluginLoadEvent;
    class var Instance : TPluginManager;
  end;

implementation

end.
UNIT settings;

{$mode objfpc}{$H+}

INTERFACE

USES
  Classes, sysutils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ComCtrls, ExtCtrls,viewWrapper,particlePhysics,Process;

TYPE

  { TSettingsForm }

  TSettingsForm = class(TForm)
    Label9: TLabel;
    RestartButton: TButton;
    flatShadingCheckBox: TCheckBox;
    Label6: TLabel;
    currentScenarioLabel: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    light2TrackBar: TTrackBar;
    setupComboBox: TComboBox;
    ScenarioProgressBar: TProgressBar;
    switchSetupButton: TButton;
    lockSetupCheckBox: TCheckBox;
    hemisphereCheckBox: TCheckBox;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    fpsTargetLabel: TLabel;
    Label2: TLabel;
    fpsMeasuredLabel: TLabel;
    Label3: TLabel;
    fpsTrackBar: TTrackBar;
    ballSizeTrackBar: TTrackBar;
    Label4: TLabel;
    Label5: TLabel;
    switchTimeLabel: TLabel;
    speedTrackBar: TTrackBar;
    switchTimeTrackBar: TTrackBar;
    light1TrackBar: TTrackBar;
    BallQualityTrackBar: TTrackBar;
    updateFPSTimer: TTimer;
    PROCEDURE BallQualityTrackBarChange(Sender: TObject);
    PROCEDURE ballSizeTrackBarChange(Sender: TObject);
    PROCEDURE hemisphereCheckBoxChange(Sender: TObject);
    PROCEDURE flatShadingCheckBoxChange(Sender: TObject);
    PROCEDURE FormCreate(Sender: TObject);
    PROCEDURE FormDestroy(Sender: TObject);
    PROCEDURE FormResize(Sender: TObject);
    PROCEDURE fpsTrackBarChange(Sender: TObject);
    PROCEDURE light1TrackBarChange(Sender: TObject);
    PROCEDURE light2TrackBarChange(Sender: TObject);
    PROCEDURE lockSetupCheckBoxChange(Sender: TObject);
    PROCEDURE RestartButtonClick(Sender: TObject);
    PROCEDURE speedTrackBarChange(Sender: TObject);
    PROCEDURE switchSetupButtonClick(Sender: TObject);
    PROCEDURE switchTimeTrackBarChange(Sender: TObject);
    PROCEDURE updateFPSTimerTimer(Sender: TObject);
  private

  public

  end;

VAR sharedViewState:^T_viewState;
    currentlyWindowed:boolean;
FUNCTION getSettingsForm:TSettingsForm;
FUNCTION isSettingsFormShowing:boolean;
IMPLEMENTATION
VAR SettingsForm: TSettingsForm=nil;
FUNCTION getSettingsForm:TSettingsForm;
  begin
    if SettingsForm=nil then SettingsForm:=TSettingsForm.create(nil);
    result:=SettingsForm;
  end;

FUNCTION isSettingsFormShowing:boolean;
  begin
    result:=(SettingsForm<>nil) and (SettingsForm.showing);
  end;

{ TSettingsForm }

PROCEDURE TSettingsForm.FormCreate(Sender: TObject);
  VAR i:longint;
  begin
    setupComboBox.items.clear;
    for i:=0 to ATTRACTION_MODE_COUNT-1 do setupComboBox.items.add(ATTRACTION_MODE_NAME[i]);
    setupComboBox.items.add('<random>');

    if currentlyWindowed
    then RestartButton.caption:='Restart in fullscreen mode'
    else RestartButton.caption:='Restart in windowed mode';

    fpsTrackBar.position:=sharedViewState^.targetFPS;
    ballSizeTrackBar.position:=round(ln(sharedViewState^.ballSize/0.01)/ln(0.2/0.01)*ballSizeTrackBar.max);
    BallQualityTrackBar.position:=sharedViewState^.ballRefinement;
    hemisphereCheckBox.checked:=sharedViewState^.hemispheres;
    flatShadingCheckBox.checked:=sharedViewState^.flatShading;
    light1TrackBar.position:=round(255*sharedViewState^.light1Brightness);
    light2TrackBar.position:=round(255*sharedViewState^.light2Brightness);

    speedTrackBar.position:=round(ln(sharedViewState^.ParticleEngine.TICKS_PER_SIMULATION_TIME_UNIT/1000)*100/ln(0.1));
    switchTimeTrackBar.position:=round(sharedViewState^.ParticleEngine.MODE_SWITCH_INTERVAL_IN_TICKS/100);
    switchTimeLabel.caption:=formatFloat('00.0',switchTimeTrackBar.position*0.1)+'s';
    lockSetupCheckBox.checked:=sharedViewState^.ParticleEngine.lockCurrentSetup;
  end;

PROCEDURE TSettingsForm.ballSizeTrackBarChange(Sender: TObject);
  begin
    //range: 0.001 - 0.2
    sharedViewState^.ballSize:=0.01*exp(ln(0.2/0.01)*ballSizeTrackBar.position/ballSizeTrackBar.max);
  end;

PROCEDURE TSettingsForm.BallQualityTrackBarChange(Sender: TObject);
  begin
    sharedViewState^.ballRefinement:=BallQualityTrackBar.position;
  end;

PROCEDURE TSettingsForm.hemisphereCheckBoxChange(Sender: TObject);
  begin
    sharedViewState^.hemispheres:=hemisphereCheckBox.checked;
  end;

PROCEDURE TSettingsForm.flatShadingCheckBoxChange(Sender: TObject);
  begin
    sharedViewState^.flatShading:=flatShadingCheckBox.checked;
  end;

PROCEDURE TSettingsForm.FormDestroy(Sender: TObject);
begin

end;

PROCEDURE TSettingsForm.FormResize(Sender: TObject);
begin

end;

PROCEDURE TSettingsForm.fpsTrackBarChange(Sender: TObject);
  begin
    sharedViewState^.targetFPS:=fpsTrackBar.position;
    fpsTargetLabel.caption:=intToStr(sharedViewState^.targetFPS);
  end;

PROCEDURE TSettingsForm.light1TrackBarChange(Sender: TObject);
  begin
    sharedViewState^.light1Brightness:=light1TrackBar.position/255;
  end;

PROCEDURE TSettingsForm.light2TrackBarChange(Sender: TObject);
  begin
    sharedViewState^.light2Brightness:=light2TrackBar.position/255;
  end;

PROCEDURE TSettingsForm.lockSetupCheckBoxChange(Sender: TObject);
  begin
    sharedViewState^.ParticleEngine.lockCurrentSetup:=lockSetupCheckBox.checked;
  end;

PROCEDURE TSettingsForm.RestartButtonClick(Sender: TObject);
  VAR Process:TProcess;
  begin
    Process:=TProcess.create(self);
    Process.options:=[poDetached];
    Process.executable:=paramStr(0);
    if not(currentlyWindowed) then Process.parameters.add('-windowed');
    sharedViewState^.saveToFile(ChangeFileExt(paramStr(0),'.settings'));
    Process.execute;
    halt;
  end;

PROCEDURE TSettingsForm.speedTrackBarChange(Sender: TObject);
  begin
    sharedViewState^.ParticleEngine.TICKS_PER_SIMULATION_TIME_UNIT:=1000*exp(speedTrackBar.position/100*ln(0.1));
  end;

PROCEDURE TSettingsForm.switchSetupButtonClick(Sender: TObject);
  begin
    if setupComboBox.ItemIndex>=0
    then sharedViewState^.ParticleEngine.nextSetup(sharedViewState^.modeTicks,setupComboBox.ItemIndex)
    else sharedViewState^.ParticleEngine.nextSetup(sharedViewState^.modeTicks);
  end;

PROCEDURE TSettingsForm.switchTimeTrackBarChange(Sender: TObject);
  begin
    sharedViewState^.ParticleEngine.MODE_SWITCH_INTERVAL_IN_TICKS:=switchTimeTrackBar.position*100;
    switchTimeLabel.caption:=formatFloat('00.0',switchTimeTrackBar.position*0.1)+'s';
  end;

PROCEDURE TSettingsForm.updateFPSTimerTimer(Sender: TObject);
  begin
    fpsMeasuredLabel.caption:=formatFloat('00.0',sharedViewState^.getFps);
    ScenarioProgressBar.position:=sharedViewState^.modeTicks;
    currentScenarioLabel.caption:=ATTRACTION_MODE_NAME[sharedViewState^.ParticleEngine.currentAttractionMode];
  end;

INITIALIZATION
  {$I settings.lrs}

FINALIZATION
  if SettingsForm<>nil then FreeAndNil(SettingsForm);

end.


// Macro for testing MPD geometry with geantino

R__ADD_INCLUDE_PATH($VMCWORKDIR)
#include "macro/mpd/geometry_stage1.C"

//___________________________________________________________
void RadLenSim(Int_t nStartEvent = 0, Int_t nEvents = 50, TString outFile = "radlen.root") {

  FairRunSim *fRun = new FairRunSim();
  
  fRun->SetName("TGeant3");
  fRun->SetSink(new FairRootFileSink(outFile));          // Output file

  geometry_stage1(fRun);    // load mpd geometries

  FairPrimaryGenerator* primGen = new FairPrimaryGenerator();
  fRun->SetGenerator(primGen); 
  
  FairBoxGenerator* boxGen = new FairBoxGenerator(0, 100); // 0 = geantino - required for RadLen Simulation
  boxGen->SetPRange(.2,.2); // GeV/c
  boxGen->SetPhiRange(0, 360); 
  boxGen->SetThetaRange(0, 180); 
  boxGen->SetXYZ(0., 0., 0.);
  primGen->AddGenerator(boxGen); 

  fRun->SetStoreTraj(kFALSE);
  fRun->SetRadLenRegister(kTRUE); // radiation length manager 

  fRun->Init(); 


  // Fill the Parameter containers for this run
  //-------------------------------------------

  FairRuntimeDb *rtdb=fRun->GetRuntimeDb();
  rtdb->print();


  fRun->Run(nEvents); 
}  

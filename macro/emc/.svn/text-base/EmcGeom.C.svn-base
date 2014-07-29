
class Mpdshape;


  ofstream* f ;
  std::ostringstream points, position, rotation;


  //=====================================================================
  //=====================================================================
  //=====================================================================


//____________________________________________________________________
int EmcGeom() {

  gROOT->ProcessLine(".include/home/boiana/mpdroot/geobase ");
 
  gROOT->LoadMacro("$VMCWORKDIR/macro/mpd/mpdloadlibs.C");

  mpdloadlibs(0,0); 
  
  gROOT->ProcessLine(".L ../mpd/geometry/mpdshape.class.C");
  
  gROOT->LoadClass("Mpdshape");

  const char* filename = "emc_tr.geo";

  f = new ofstream(filename, ios::out | ios::trunc);

  gROOT->ProcessLine(".L EmcGeom.cxx");

  EmcGeom* emc = new EmcGeom(f);

  emc->BuildEMC();
  
  f->close(); 

}

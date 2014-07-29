{
#include "mpdshape.class.C"


  const char* filename = "tpc_v2.geo";

  Double_t innerRadius = 270.0; //
  Double_t outerRadius = 1100.0; //


  Double_t tpc_z = .5*3400.0; // 
  Double_t tpc_drift_z = .5*3000.0; // 

  Double_t thick_al = .05; //
  Double_t thick_tedlar = .05; //
  Double_t thick_kevlar = .6; //
  Double_t thin_kevlar = .2; //


  // Outer wall
  Double_t honeycomb_out = 30.; //
  Double_t honeycomb_mid = 20.; //
  Double_t co2_out = 50.; //

  // Inner wall
  Double_t honeycomb_in = 10.; //
  Double_t co2_in = 50.; //

  // output file
  ofstream* f = new ofstream(filename, ios::out | ios::trunc);                                                                            

  // helper streams
  std::ostringstream points, position, rotation;
  points.setf(ios::showpoint); points.setf(ios::fixed);
  points << setprecision(6);
  position.setf(ios::showpoint); position.setf(ios::fixed);
  position << setprecision(6);
  rotation.setf(ios::showpoint); rotation.setf(ios::fixed);
  rotation << setprecision(6);

  // tpc vessel tube
  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_z << endl;
  points <<  innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_z;

  // std::cout << points.str() << endl;

  // tpc tube definition
  Mpdshape* tube = new Mpdshape(f, "tpcChamber1", "cave", "TUBE",
				"air", points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

  // tpc inner wall al

  Double_t R_cur = innerRadius + thick_al;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  innerRadius << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  // std::cout << points.str() << endl;

  tube->SetVolumeName("tpc01iw1");
  tube->SetMotherVolumeName("tpcChamber1");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall inner tube

  Double_t R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  // std::cout << points.str() << endl;

  tube->SetVolumeName("tpc01iw2");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall inner tube

  R_cur_old = R_cur;
  R_cur = R_cur + thick_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  // std::cout << points.str() << endl;

  tube->SetVolumeName("tpc01iw3");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc inner wall honeycomb

  R_cur_old = R_cur;
  R_cur = R_cur + honeycomb_in;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  // std::cout << points.str() << endl;

  tube->SetVolumeName("tpc01iw4");
  tube->SetMedia("nomex");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc inner wall kevlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw5");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc  inner wall tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw6");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc inner wall al

  R_cur_old = R_cur;
  R_cur = R_cur + thick_al;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw7");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

  // tpc co2 gap

  R_cur_old = R_cur;
  R_cur = R_cur + co2_in;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw8");
  tube->SetMedia("CO2");
  tube->SetPoints(points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

  // tpc inner wall tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw9");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

  // tpc inner wall kevlar

  R_cur_old = R_cur;
  R_cur = R_cur + thin_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw10");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

   // tpc honeycomb

  R_cur_old = R_cur;
  R_cur = R_cur + honeycomb_in;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw11");
  tube->SetMedia("nomex");
  tube->SetPoints(points.str());
  tube->SetSegment(0);
  tube->DumpToFile();

  // tpc wall kevlar tube

  R_cur_old = R_cur;
  R_cur = R_cur + thin_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw12");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall inner tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01iw13");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc drift volume

  Double_t drift_rad = 998.1;

  R_cur_old = R_cur;
  //  R_cur = R_cur + drift_rad;
  R_cur = drift_rad;

  Double_t pad_in = R_cur_old;
  Double_t pad_out = R_cur;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01sv");
  tube->SetMedia("TPCmixture");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc outer wall tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow1");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc outer wall kevlar

  R_cur_old = R_cur;
  R_cur = R_cur + thin_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow2");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();


   // tpc honeycomb

  R_cur_old = R_cur;
  R_cur = R_cur + honeycomb_mid;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow3");
  tube->SetMedia("nomex");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall kevlar tube

  R_cur_old = R_cur;
  R_cur = R_cur + thin_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow4");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall outer tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow5");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

   // tpc co2 gap

  R_cur_old = R_cur;
  R_cur = R_cur + co2_out;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow6");
  tube->SetMedia("CO2");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc outer wall aluminium

  R_cur_old = R_cur;
  R_cur = R_cur + thick_al;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow7");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall outer tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow8");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall kevlar tube

  R_cur_old = R_cur;
  R_cur = R_cur + thick_kevlar;

  points.str(""); position.str(""); rotation.str("");

  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow9");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

   // tpc outer wall honeycomb

  R_cur_old = R_cur;
  R_cur = R_cur + honeycomb_out;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow10");
  tube->SetMedia("nomex");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc outer wall kevlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_kevlar;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow11");
  tube->SetMedia("kevlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc wall outer tedlar

  R_cur_old = R_cur;
  R_cur = R_cur + thick_tedlar;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow12");
  tube->SetMedia("tedlar");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // tpc al wall

  R_cur_old = R_cur;
  R_cur = R_cur + thick_al;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  R_cur_old << " " << R_cur << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z;

  tube->SetVolumeName("tpc01ow13");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  if (R_cur > outerRadius ) cout << "!!!! something wrong !!!!\n"; 
  cout << R_cur << " " << outerRadius << endl;


  // ============ pad plane South

  Double_t pad_plane = 2.;   // 
  Double_t back_plane = 3.;   // 

  Double_t Z_ext = tpc_drift_z - pad_plane - back_plane;
  Double_t Z_inn = tpc_drift_z - back_plane;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << Z_ext << endl;
  points <<  pad_in << " " << pad_out << endl;
  points << 0.0 << " " << 0.0 << " " << Z_inn;

  tube->SetVolumeName("tpc01ppS");
  tube->SetMotherVolumeName("tpc01sv");
  tube->SetMedia("G10");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // pad plane North

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << -Z_ext << endl;
  points <<  pad_in << " " << pad_out << endl;
  points << 0.0 << " " << 0.0 << " " << -Z_inn;

  tube->SetVolumeName("tpc01ppN");
  tube->SetMedia("G10");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al back plane South

  Z_ext = tpc_drift_z - back_plane;
  Z_inn = tpc_drift_z;

  points.str(""); position.str(""); rotation.str("");
  points << 0.0 << " " << 0.0 << " " << Z_ext << endl;
  points <<  pad_in << " " << pad_out << endl;
  points << 0.0 << " " << 0.0 << " " << Z_inn;

  tube->SetVolumeName("tpc01bpS");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al back plane North

  points.str("");
  points << 0.0 << " " << 0.0 << " " << -Z_ext << endl;
  points <<  pad_in << " " << pad_out << endl;
  points << 0.0 << " " << 0.0 << " " << -Z_inn;

  tube->SetVolumeName("tpc01bpN");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al flanch  outer South

  points.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  pad_out << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z + 15.;

  tube->SetMotherVolumeName("tpcChamber1");
  tube->SetVolumeName("tpc01ofS");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al flanch outer North

  points.str("");
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z << endl;
  points <<  pad_out << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z - 15.;

  tube->SetVolumeName("tpc01ofN");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al flanch  inner South

  points.str("");
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z << endl;
  points <<  innerRadius << " " << pad_in << endl;
  points << 0.0 << " " << 0.0 << " " << tpc_drift_z + 15.;

  tube->SetVolumeName("tpc01ifS");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();

  // al flanch inner North

  points.str("");
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z << endl;
  points <<  innerRadius << " " << pad_in << endl;
  points << 0.0 << " " << 0.0 << " " << -tpc_drift_z - 15.;

  tube->SetVolumeName("tpc01ifN");
  tube->SetMedia("aluminium");
  tube->SetPoints(points.str());
  tube->DumpToFile();


#if 1

 // ====== Ribs

  Double_t xWidth = 0.5*50.0; // x - half-dimension of block
  Double_t yWidth = .5*(TMath::Sqrt(outerRadius*outerRadius - xWidth*xWidth) -
			innerRadius); // y - half-dimension of block
  Double_t zWidth = .5*95.0; // z - half-dimension of block

  Double_t kz = tpc_drift_z + 15. + zWidth;

  // tpc ribs 
  points.str(""); position.str(""); rotation.str("");
  points << xWidth << " " << -yWidth << " " << -zWidth << endl;
  points << xWidth << " " << yWidth << " " << -zWidth << endl;
  points << -xWidth << " " << yWidth << " " << -zWidth << endl;
  points << -xWidth << " " << -yWidth << " " << -zWidth << endl;

  points << xWidth << " " << -yWidth << " " << zWidth << endl;
  points << xWidth << " " << yWidth << " " << zWidth << endl;
  points << -xWidth << " " << yWidth << " " << zWidth << endl;
  points << -xWidth << " " << -yWidth << " " << zWidth;

  position << 0.0 << " " << 0.0 << " " << 0.0;

  const Double_t center_rad = innerRadius + yWidth;
  cout << "center_rad: " << center_rad << "\n";

  Mpdshape* tpcrib = new Mpdshape(f, "tpc01Rib", "tpcChamber1", "BOX", "aluminium",
				   points.str(), position.str());
  tpcrib->SetSegment(1);

  // rib gaps  helpers 
  zWidth = zWidth - 15.;
  yWidth = yWidth - 50.; 

  points.str(""); position.str(""); rotation.str("");
  points << xWidth << " " << -yWidth << " " << -zWidth << endl;
  points << xWidth << " " << yWidth << " " << -zWidth << endl;
  points << -xWidth << " " << yWidth << " " << -zWidth << endl;
  points << -xWidth << " " << -yWidth << " " << -zWidth << endl;

  points << xWidth << " " << -yWidth << " " << zWidth << endl;
  points << xWidth << " " << yWidth << " " << zWidth << endl;
  points << -xWidth << " " << yWidth << " " << zWidth << endl;
  points << -xWidth << " " << -yWidth << " " << zWidth;

  position << 0.0 << " " << 0.0 << " " << 0.0;


  Mpdshape* tpcgap = new Mpdshape(f, "tpc01Gap", "tpcChamber1", "BOX", "air",
				   points.str(), position.str());
  tpcgap->SetSegment(1);

  const Double_t step = 30.;
  const Double_t phi_step = TMath::DegToRad()*step;

  for (Int_t j = 0; j < 2; j++) {
    if (j > 0) kz =-kz; 
    for (Int_t k = 0; k < int(360./step); k++) {
      Double_t phi = k*phi_step;
      
      Double_t kx = center_rad* TMath::Cos(phi);
      Double_t my = center_rad* TMath::Sin(phi);
      tpcrib->SetPosition(kx, my, kz);
      tpcrib->SetRotation((step*k - 90.), 0.0, 0.0);
      tpcrib->DumpWithIncrement();
      std::string name = tpcrib->GetVolumeName();
      name += "#";
      name += itoa(tpcrib->GetSegment() - 1);

      if (k == 0 && j == 0) {
	// Only one copy enough
	tpcgap->SetMotherVolumeName(name);
	tpcgap->SetPosition(0,0,0);
	tpcgap->SetRotation(0.0, 0.0, 0.0);
	tpcgap->DumpWithIncrement();
      }
    }
  }  
                            
#endif

  // close geometry file                                                       
  f->close(); 

}

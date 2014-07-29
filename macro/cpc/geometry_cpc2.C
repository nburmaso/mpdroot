{
#include "../mpd/geometry/mpdshape.class.C"


  const char* filename = "cpc.geo";
  Double_t initDist = 1735.0; // Distance from interacting point, mm

  const Double_t innerRadius = 150.0; // Layer inner radius, mm
  const Double_t outerRadius = 500.0; // Layer outer radius, mm

//   Double_t xWidth = 10.0; // x - half-dimension of block
//   Double_t yWidth = 10.0; // y - half-dimension of block
//   Double_t zWidth = 5.0; // z - half-dimension of block
  Double_t layerThickness = 70.0; 

  ////////////   output file for straw endcap geometry   ///////////////////////////////////////
  ofstream* f = new ofstream(filename, ios::out | ios::trunc);  
                                                                            

  ////////////   helper streams    ////////////////////////////////////////////////////////////
  std::ostringstream points, position, rotation;
  points.setf(ios::showpoint); points.setf(ios::fixed); points << setprecision(6);
  position.setf(ios::showpoint); position.setf(ios::fixed); rotation << setprecision(6);
  rotation.setf(ios::showpoint); rotation.setf(ios::fixed); rotation << setprecision(6);
  


  ////////////   cpc helpers  /////////////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -layerThickness/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << layerThickness/2.0;
  position << 0.0 << " " << 0.0 << " " << initDist;
  
  ////////////    cpc definition    /////////////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01l", "cave", "TUBE", "air",                                                                        
				   points.str(), position.str());                       
  stofbox->SetSegment(1);
  stofbox->DumpWithIncrement();

  stofbox->SetRotation(0.0,180.0,0.0);
  stofbox->SetPosition(0.0,0.0,-initDist);
  stofbox->DumpWithIncrement();

  stofbox->SetRotation(0.0,180.0,0.0);
  stofbox->SetPosition(0.0,0.0,-initDist-320.0);
  stofbox->DumpWithIncrement();

  stofbox->SetPosition(0.0,0.0,initDist+320.0);
  stofbox->DumpWithIncrement();
  
  

  ////////////     panel helpers    /////////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -10/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10/2.0;
  position << 0.0 << " " << 0.0 << " " << -10.0; 

  ////////////     panel definition     ////////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sl", "cpc01l", "TUBE", "air",				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

  for (Int_t i = 1; i < 2; i++)
    {
      stofbox->SetPosition(0.0,0.0,-10.0+20.0*i);
      stofbox->DumpWithIncrement();
    }
    
    
    
    
  ///////////     arco centr-volume      ////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -10/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10/2.0;
//  position << 0.0 << " " << 0.0 << " " << -20.0;
  
  ///////////     arco centr-volum definition     ///////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01al", "cpc01l", "TUBE", "arco28020", 				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
//  stofbox->DumpWithIncrement();
  
    for (Int_t i = -1; i < 2; i++)
    {
      stofbox->SetPosition(0.0, 0.0, 20.0*i);
      stofbox->DumpWithIncrement();
    }
    
    
    
    
  ///////////    base helpers     ///////////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << -9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << 9.19/2.0;

  ///////////    bace definition     ////////////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osn", "cpc01sl", "TUBE", "G10",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ///////////    layer from rohacell rigth helpers     /////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << -9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << -9.99/2.0;

  ///////////    layer from rohacell right definition     //////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osnrh1", "cpc01sl", "TUBE", "rohacellhf71",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ///////////    layer from rohacell left helpers     //////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << 9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << 9.99/2.0;

  ///////////    layer from rohacell left definition     ///////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osnrh2", "cpc01sl", "TUBE", "rohacellhf71",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  //////////     volum for R pads      /////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -9.99/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << -10.0/2.0;
  
  //////////     volum for R pads definition      //////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01vrl", "cpc01sl", "TUBE", "air",																	    
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  /////////      volum for Fi pads       //////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << 9.99/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10.0/2.0;


  ///////////    volum for Fi pads definition     /////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01vfl", "cpc01sl", "TUBE", "air",																	    
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

    
    

#if (1)
  ///////////    sector in Fi helpers     /////////////////////////////////////////////////////
  points.str("");position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << innerRadius << " " << outerRadius << endl;
  points << 9.99/2.0 << " " << innerRadius << " " << outerRadius;

  ///////////    sector in Fi definition     //////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sectu", "cpc01vfl", "PGON", "air",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

  Double_t angle = 0.0;
  for(Int_t i = 1; i < 320; i++)
    {
      angle += 1.125;
      stofbox->SetRotation(0.0,0.0,angle);
      stofbox->DumpWithIncrement();
    }


  ///////////    padFi1 helpers     //////////////////////////////////////////////////////////
  Double_t padMovX = 0.2/TMath::Sin(1.125*TMath::DegToRad());
  Double_t innerRadiusPad = innerRadius - padMovX;
  Double_t outerRadiusPad = outerRadius - padMovX;
  
  points.str(""); position.str("");
  points << "2" << endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << innerRadiusPad << " " << 200.0 - padMovX << endl;
  points << 9.99/2.0 << " " << innerRadiusPad << " " << 200.0 - padMovX;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////      padFi1 definition     ////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01padR1", "cpc01sectu", "PGON", "copper",                                                                        
				   points.str(), position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ///////////      padFi2 helpers     //////////////////////////////////////////////////////
  points.str(""); position.str("");
  points << "2" << endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << 205.0 - padMovX << " " << 300.0 - padMovX << endl;
  points << 9.99/2.0 << " " << 205.0 - padMovX << " " << 300.0 - padMovX;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////      padFi2 definition     ///////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01padR2", "cpc01sectu", "PGON", "copper",                                                                        
				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  



  ///////////      padFi3 helpers     //////////////////////////////////////////////////////
  points.str("");position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << 305.0 - padMovX << " " << outerRadiusPad << endl;
  points << 9.99/2.0 << " " << 305.0 - padMovX << " " << outerRadiusPad;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////      padFi3 definition     ///////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01padR3", "cpc01sectu", "PGON", "copper",                                                                        
				   points.str(), position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ///////////     sector in R helpers     //////////////////////////////////////////////////
  points.str("");position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 10.0 << " " << 4 << endl;
  points << -10.0/2.0 << " " << innerRadius << " " << outerRadius << endl;
  points << -9.99/2.0 << " " << innerRadius << " " << outerRadius;

  ///////////     sector in R definition     //////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sectd", "cpc01vrl", "PGON", "air",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

  Double_t angle = 0.0;
  for(Int_t i = 1; i < 36; i++)
    {
      angle += 10.0;
      stofbox->SetRotation(0.0,0.0,angle);
      stofbox->DumpWithIncrement();
    }

  std::string name;
  Double_t phiMovX = 0.2/TMath::Sin(10.0*TMath::DegToRad());
  Double_t innerRadiusPhi = innerRadius - phiMovX;
  Double_t outerRadiusPhi = outerRadius - phiMovX;
  Double_t step = (outerRadiusPhi - innerRadiusPhi)/31.0;

  for (Int_t i=0; i<31; i++)
    {
      name = "cpc01padPhi";
      name += itoa(i);
      points.str(""); position.str("");
      points << "2" << endl;
      points << 0.0 << " " << 10.0 << " " << 4 << endl;
      points << -10.0/2.0 << " " << innerRadiusPhi + step * i + 0.2 << " " << innerRadiusPhi + step * (i+1) << endl;
      points << -9.99/2.0 << " " << innerRadiusPhi + step * i + 0.2 << " " << innerRadiusPhi + step * (i+1);
      position << phiMovX << " " << 0.0 << " " << 0.0;

      // padPhi layer definition
      Mpdshape* stofbox = new Mpdshape(f, name, "cpc01sectd", "PGON", "copper",                                                                        
			points.str(),position.str());                       
      stofbox->SetSegment(1);
      stofbox->SetMotherSegment(1);
      stofbox->DumpWithIncrement();	
    }
    
    
    

  //*****************************************************************************************************************
  ///////////     panel helpers     /////////////////////////////////////////////////////
  points.str(""); position.str("");
  points << 0. << " " << 0. << " " << -10/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10/2.0;
  position << 0.0 << " " << 0.0 << " " << -30.0; 

  ///////////     panel definition     ////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sl2", "cpc01l", "TUBE", "air",                                                                        
				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ///////////     base helpers     ////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << -9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << 9.19/2.0;

  ///////////     base definition     ////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osn2", "cpc01sl2", "TUBE", "G10",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ///////////     layer from rohacell left helpers     ///////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << 9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << 9.99/2.0;

  ///////////     layer from rohacell left definition     ////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osnrh4", "cpc01sl2", "TUBE", "rohacellhf71",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  /////////      volum for Fi pads       //////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << 9.99/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10.0/2.0;
  position << 0.0 << " " << 0.0 << " " << -10.0; 

  ///////////    volum for Fi pads definition     /////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01vfll", "cpc01sl2", "TUBE", "air",																	    
				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ///////////     sector helpers     /////////////////////////////////////////////////
  points.str(""); position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << innerRadius << " " << outerRadius << endl;
  points << 9.99/2.0 << " " << innerRadius << " " << outerRadius;

  ///////////     sector definition     //////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sect2u", "cpc01vfll", "PGON", "air",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

  Double_t angle = 0.0;
  for(Int_t i = 1; i < 320; i++)
    {
      angle += 1.125;
      stofbox->SetRotation(0.0,0.0,angle);
      stofbox->DumpWithIncrement();
    }
  
  
  
  
  ///////////     padFi1 helpers     /////////////////////////////////////////////////
  Double_t padMovX = 0.2/TMath::Sin(1.125*TMath::DegToRad());
  Double_t innerRadiusPad = innerRadius - padMovX;
  Double_t outerRadiusPad = outerRadius - padMovX;

  points.str(""); position.str("");
  points << "2" << endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << innerRadiusPad << " " << 200.0 - padMovX << endl;
  points << 9.99/2.0 << " " << innerRadiusPad << " " << 200.0 - padMovX;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////     padFi1 definition      /////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01pad2R1", "cpc01sect2u", "PGON", "copper",                                                                        
				   points.str(), position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ///////////     padFi2 helpers      ///////////////////////////////////////////////
  points.str(""); position.str("");
  points << "2" << endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << 205.0 - padMovX << " " << 300.0 - padMovX << endl;
  points << 9.99/2.0 << " " << 205.0 - padMovX << " " << 300.0 - padMovX;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////     padFi2 definition     /////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01pad2R2", "cpc01sect2u", "PGON", "copper",                                                                        
				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ///////////     padFi3 helpers     ///////////////////////////////////////////////
  points.str("");position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 1.125 << " " << 1 << endl;
  points << 10.0/2.0 << " " << 305.0 - padMovX << " " << outerRadiusPad << endl;
  points << 9.99/2.0 << " " << 305.0 - padMovX << " " << outerRadiusPad;
  position << padMovX << " " << 0.0 << " " << 0.0;

  ///////////     padFi3 definition      ///////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01pad2R3", "cpc01sect2u", "PGON", "copper",                                                                        
				   points.str(), position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
   
   

  //***********************************************************************************************
  ////////////    panel helpers      /////////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -10/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << 10/2.0;
  position << 0.0 << " " << 0.0 << " " << 30.0; 
  
  ////////////    panel definition    ////////////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sl3", "cpc01l", "TUBE", "air",                                                                        
				   points.str(),position.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ////////////    base helpers    ////////////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << -9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << 10.0/2.0;

  ////////////    base definition    /////////////////////////////////////////////////////////////    
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osn3", "cpc01sl3", "TUBE", "G10",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  ////////////    layer from rohacell rigth helpers     //////////////////////////////////////////
  points.str("");position.str("");
  points << 0.0 << " " << 0.0 << " " << -9.19/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0.0 << " " << 0.0 << " " << -9.99/2.0;

  ////////////    layer from rohacell right definition     ///////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01osnrh5", "cpc01sl3", "TUBE", "rohacellhf71",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  
  
  //////////     volum for R pads      /////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -9.99/2.0 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << -10.0/2.0;
  
  //////////     volum for R pads definition      //////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01vrll", "cpc01sl3", "TUBE", "air",																	    
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();
  
  
  

  ////////////    sector in R helpers    /////////////////////////////////////////////////////////
  points.str("");position.str("");
  points << "2" <<endl;
  points << 0.0 << " " << 10.0 << " " << 4 << endl;
  points << -10.0/2.0 << " " << innerRadius << " " << outerRadius << endl;
  points << -9.99/2.0 << " " << innerRadius << " " << outerRadius;

  ////////////    sector in R definition     /////////////////////////////////////////////////////
  Mpdshape* stofbox = new Mpdshape(f, "cpc01sect3d", "cpc01vrll", "PGON", "air",                                                                        
				   points.str());                       
  stofbox->SetSegment(1);
  stofbox->SetMotherSegment(1);
  stofbox->DumpWithIncrement();

  Double_t angle = 0.0;
  for(Int_t i = 1; i < 36; i++)
    {
      angle += 10.0;
      stofbox->SetRotation(0.0,0.0,angle);
      stofbox->DumpWithIncrement();
    }


  
  ///////////     R pads     /////////////////////////////////////////////////////////////////////
  std::string name;
  Double_t phiMovX = 0.2/TMath::Sin(10.0*TMath::DegToRad());
  Double_t innerRadiusPhi = innerRadius - phiMovX;
  Double_t outerRadiusPhi = outerRadius - phiMovX;
  Double_t step = (outerRadiusPhi - innerRadiusPhi)/31.0;

  for (Int_t i=0; i<31; i++)
    {
      name = "cpc01pad3Phi";
      name += itoa(i);
      points.str(""); position.str("");
      points << "2" << endl;
      points << 0.0 << " " << 10.0 << " " << 4 << endl;
      points << -10.0/2.0 << " " << innerRadiusPhi + step * i + 0.2 << " " << innerRadiusPhi + step * (i+1) << endl;
      points << -9.99/2.0 << " " << innerRadiusPhi + step * i + 0.2 << " " << innerRadiusPhi + step * (i+1);
      position << phiMovX << " " << 0.0 << " " << 0.0;

      // straw layer definition
      Mpdshape* stofbox = new Mpdshape(f, name, "cpc01sect3d", "PGON", "copper",                                                                        
				       points.str(),position.str());                       
      stofbox->SetSegment(1);
      stofbox->SetMotherSegment(1);
      stofbox->DumpWithIncrement();	
    }
  //**************************************************************************************
                        
#endif
  //////////// close geometry file                                                                                                                                                                        
  f->close(); 
}

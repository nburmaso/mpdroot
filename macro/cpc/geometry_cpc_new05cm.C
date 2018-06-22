#include "../mpd/geometry/mpdshape.class.C"

void geometry_cpc_new05cm(){


  //const char* filename = "cpc.geo";
  const char* filename = "cpc2.geo";
  //Double_t initdist = 1745.0; // parameter for calculation of distance from interaction vertex, mm
  Double_t initdist = 1725.0; // parameter for calculation of distance from interaction vertex, mm
  Double_t cpcposition; // Distance from interaction vertex, mm

  const Double_t innerRadius = 150.0; // inner radius, mm
  //const Double_t Radius1 = 200.0; // inner radius, mm
  //const Double_t Radius1 = 280.0; // inner radius, mm
  const Double_t Radius1 = 400.0; // inner radius, mm
  //const Double_t Radius2 = 300.0; // inner radius, mm
  //const Double_t Radius2 = 450.0; // inner radius, mm
  const Double_t outerRadius = 650.0; // outer radius, mm

  Double_t outerlayerwidth = 30.0; // mm
  //Double_t detectorwidth = 90.0; // mm
  Double_t detectorwidth = 70.0; // mm
  Double_t panelthickness = 10.0; // mm
  Double_t gapthickness = 5.0; // mm
  Double_t halfpanelthkns = panelthickness/2.; // mm
  Double_t halfgapthkns = gapthickness/2.; // mm
  Int_t numberoflayers = 9;
  Int_t numberofpanels = 5;
  Int_t numberofgaps = 4;
  Double_t fourlayerthickness = (Double_t(numberofpanels-1)/2.)*panelthickness
+(Double_t(numberofgaps)/2.)*gapthickness;
  //Double_t distbetweencpcs = 320.0; // mm
  Double_t distbetweencpcs = 330.0; // mm
  Double_t copperthickness = 0.018; // mm
  Double_t cfcthickness = 0.5; // mm
  Double_t g10thickness = 0.5; // mm
  Double_t rohacellthickness = panelthickness-2.*copperthickness-2.*cfcthickness-2.*g10thickness;   // mm
  Double_t halfrohacellthkns=rohacellthickness/2.;
  Double_t rohcfc = rohacellthickness+2.*cfcthickness;   // mm
  Double_t halfrohcfc = rohcfc/2.;   // mm
  Double_t rohcfcg10 = rohcfc+2.*g10thickness;   // mm
  Double_t halfrohcfcg10 = rohcfcg10/2.;   // mm
  //Double_t rohcfcg10copper = rohcfcg10+2.*copperthickness;   // mm
  //Double_t halfrohcfcg10copper = rohcfcg10copper/2.;   // mm
  //Int_t phipadsnumber1=576;
  Int_t phipadsnumber1=512;
  Int_t rpadsnumber1=2;
  //Int_t phipadsnumber2=18;
  Int_t phipadsnumber2=12;
  //Int_t rpadsnumber2=96;
  Int_t rpadsnumber2=128;
  //Double_t delR1=2.5; // mm
  Double_t delR1=0.25; // mm
  Double_t delfi1=Double_t(360)/Double_t(phipadsnumber1); // grad
  Double_t delfi2=Double_t(360)/Double_t(phipadsnumber2); // grad 
  //Double_t delr2=(outerRadius-innerRadius-4.*delR1)/Double_t(rpadsnumber2); // mm 
  Double_t delr2=(outerRadius-innerRadius)/Double_t(rpadsnumber2); // mm 
  
  Double_t delfi1_frac=0.98; // 
  Double_t delfi2_frac=0.98; // 
  Double_t delr2_frac=0.98; // 



  Double_t cpcposmflt; // mm

  /*char cpcparttmp[20];
  char cpclayername[20];
  char stmp[20];
  char cpcpart[20];// combined title: cpcprt + part number

  strcpy(cpcparttmp,"cpc01prt");*/

  ofstream* f = new ofstream(filename, ios::out | ios::trunc);


  ////////////   helper streams    ////////////////////////////////////////////////////////////
  std::ostringstream points, position, rotation;
  points.setf(ios::showpoint); points.setf(ios::fixed); points << setprecision(6);
  position.setf(ios::showpoint); position.setf(ios::fixed); rotation << setprecision(6);
  rotation.setf(ios::showpoint); rotation.setf(ios::fixed); rotation << setprecision(6);

  ////////////   cpc helpers  ///////////////////////////////////////////////
  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -detectorwidth/2.0 << endl;
  points << innerRadius-outerlayerwidth << " " << outerRadius+outerlayerwidth << endl;
  points << 0. << " " << 0. << " " << detectorwidth/2.0;
  //position << 0.0 << " " << 0.0 << " " << initdist;

  ////////////    cpc definition    ////////////////////////////////////////
  Mpdshape* cpcbox0 = new Mpdshape(f, "cpc01part", "cave", "TUBE", "air", points.str());   
  //Mpdshape* cpcbox0 = new Mpdshape(f, "cpc01part", "cave", "TUBE", "air", points.str(),position.str());   



   /*std::ostringstream ostr;
   ostr << j;
   //std::string s = ostr.str();
   strcpy(stmp, ostr.str().c_str());
   strcpy(cpcpart,strcat(cpcparttmp,stmp));
   cout<<cpcpart<<endl;*/ 

  
     cpcbox0->SetPosition(0.0,0.0,initdist);
     cpcbox0->SetSegment(1);
     cpcbox0->DumpWithIncrement();
     //cpcposition=-initdist;
     cpcbox0->SetRotation(0.0,180.0,0.0);
     cpcbox0->SetPosition(0.0,0.0,-initdist);
     cpcbox0->DumpWithIncrement();
     //cpcposition=-initDist-distbetweencpcs;
     cpcbox0->SetPosition(0.0,0.0,-initdist-distbetweencpcs);
     cpcbox0->DumpWithIncrement();
     //cpcposition=initDist+distbetweencpcs;
     cpcbox0->SetRotation(0.0,180.0,0.0);
     cpcbox0->SetPosition(0.0,0.0,initdist+distbetweencpcs);
     cpcbox0->DumpWithIncrement();



      //cpcposmflt=initdist-fourlayerthickness;
      ////////////     panel helpers    ////////////////////////////////////
      points.str("");position.str("");
      points << 0. << " " << 0. << " " << -halfpanelthkns << endl;
      points << innerRadius-outerlayerwidth << " " << outerRadius+outerlayerwidth << endl;
      points << 0. << " " << 0. << " " << halfpanelthkns;
      //position << 0.0 << " " << 0.0 << " " << cpcposition-fourlayerthickness;
      //position << 0.0 << " " << 0.0 << " " << cpcposmflt;
      position << 0.0 << " " << 0.0 << " " << -fourlayerthickness;

      ////////////     panel definition     ///////////////////////////////

      Mpdshape* cpcbox1 = new Mpdshape(f, "cpc01slice1", "cpc01part", "TUBE", "air", points.str(),position.str());

      cpcbox1->SetMotherSegment(1);
      //cpcbox1->SetPosition(0.0,0.0,cpcposmflt+layerthickness*Double_t(i));
      //cpcbox1->SetPosition(0.0,0.0,cpcposmflt);
      cpcbox1->SetSegment(1);
      cpcbox1->DumpWithIncrement();

      points.str("");position.str("");
      points << 0. << " " << 0. << " " << -halfgapthkns << endl;
      points << innerRadius-outerlayerwidth << " " << outerRadius+outerlayerwidth << endl;
      points << 0. << " " << 0. << " " << halfgapthkns;
      //position << 0.0 << " " << 0.0 << " " << cpcposmflt+layerthickness;
      position << 0.0 << " " << 0.0 << " " << -fourlayerthickness+panelthickness/2.+gapthickness/2.;

      Mpdshape* cpcbox2 = new Mpdshape(f, "cpc01slice2", "cpc01part", "TUBE", "air", points.str(),position.str());

      cpcbox2->SetMotherSegment(1);
      //cpcbox2->SetPosition(0.0,0.0,cpcposmflt+layerthickness);
      //cpcbox2->SetPosition(0.0,0.0,cpcposmflt);
      cpcbox2->SetSegment(1);
      cpcbox2->DumpWithIncrement();

//inner part

// 4 arco2(8020) layers 

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfgapthkns << endl;
        points << innerRadius << " " << outerRadius << endl;
        points << 0. << " " << 0. << " " << halfgapthkns;
        Mpdshape* cpcbox2gas = new Mpdshape(f, "cpc01gas", "cpc01slice2", "TUBE", "arco28020",points.str());                       
        //cpcbox2gas->SetPosition(0.0,0.0,cpcposmflt+layerthickness);
        cpcbox2gas->SetPosition(0.0,0.0,0.0);
        cpcbox2gas->SetSegment(1);
        cpcbox2gas->SetMotherSegment(1);
        cpcbox2gas->DumpWithIncrement();

// 5 rohacell+carbon fiber+copper layers 

        //rohacell points                  
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -rohacellthickness/2.0 << endl;
        points << innerRadius << " " << outerRadius << endl;
        points << 0. << " " << 0. << " " << rohacellthickness/2.0;
        Mpdshape* cpcbox2roh = new Mpdshape(f, "cpc01roh", "cpc01slice1", "TUBE", "rohacellhf71",points.str());     
        //cpcbox2roh->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2roh->SetPosition(0.0,0.0,0.0);
        cpcbox2roh->SetSegment(1);
        cpcbox2roh->SetMotherSegment(1);
        cpcbox2roh->DumpWithIncrement();

        //carbon fiber points                  
	points.str("");position.str("");
	points << 0. << " " << 0. << " " << -halfrohcfc<< endl;
	points << innerRadius << " " << outerRadius << endl;
	points << 0. << " " << 0. << " " << -halfrohacellthkns;
        Mpdshape* cpcbox2cfcm = new Mpdshape(f, "cpc01cfcm", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcm->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcm->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcm->SetSegment(1);
        cpcbox2cfcm->SetMotherSegment(1);
        cpcbox2cfcm->DumpWithIncrement();
	points.str("");position.str("");
	points << 0. << " " << 0. << " " << halfrohacellthkns<< endl;
	points << innerRadius << " " << outerRadius << endl;
	points << 0. << " " << 0. << " " << halfrohcfc;
        Mpdshape* cpcbox2cfcp = new Mpdshape(f, "cpc01cfcp", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcp->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcp->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcp->SetSegment(1);
        cpcbox2cfcp->SetMotherSegment(1);
        cpcbox2cfcp->DumpWithIncrement();

        //G10 points                  
	points.str("");position.str("");
	points << 0. << " " << 0. << " " << -halfrohcfcg10 << endl;
	points << innerRadius << " " << outerRadius << endl;
	points << 0. << " " << 0. << " " << -halfrohcfc;
        Mpdshape* cpcbox2g10m = new Mpdshape(f, "cpc01g10m", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10m->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10m->SetPosition(0.0,0.0,0.0);
        cpcbox2g10m->SetSegment(1);
        cpcbox2g10m->SetMotherSegment(1);
        cpcbox2g10m->DumpWithIncrement();
	points.str("");position.str("");
	points << 0. << " " << 0. << " " << halfrohcfc << endl;
	points << innerRadius << " " << outerRadius << endl;
	points << 0. << " " << 0. << " " << halfrohcfcg10;
        Mpdshape* cpcbox2g10p = new Mpdshape(f, "cpc01g10p", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10p->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10p->SetPosition(0.0,0.0,0.0);
        cpcbox2g10p->SetSegment(1);
        cpcbox2g10p->SetMotherSegment(1);
        cpcbox2g10p->DumpWithIncrement();


        //copper pads 
         
  //////////     volume for R pads      //////////////////////////////

  points.str("");position.str("");
  points << 0. << " " << 0. << " " << -halfpanelthkns << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << -halfrohcfcg10;
  
  //////////     volume for R pads definition      ///////////////////

  Mpdshape* cpcbox2ru = new Mpdshape(f, "cpc01vrl", "cpc01slice1", "TUBE", "air", points.str());      
                 
  cpcbox2ru->SetPosition(0.0,0.0,0.0);
  cpcbox2ru->SetSegment(1);
  cpcbox2ru->SetMotherSegment(1);
  cpcbox2ru->DumpWithIncrement();

 /////////      volume for Fi pads       //////////////////////////////

  points.str("");position.str("");
  points << 0. << " " << 0. << " " << halfrohcfcg10 << endl;
  points << innerRadius << " " << outerRadius << endl;
  points << 0. << " " << 0. << " " << halfpanelthkns;


  ///////////    volume for Fi pads definition     ////////////////////

  Mpdshape* cpcbox2fiu = new Mpdshape(f, "cpc01vfl", "cpc01slice1", "TUBE", "air", points.str());                       

  cpcbox2fiu->SetPosition(0.0,0.0,0.0);
  cpcbox2fiu->SetSegment(1);
  cpcbox2fiu->SetMotherSegment(1);
  cpcbox2fiu->DumpWithIncrement();

  ///////////    sector in Fi helpers     ////////////////////////////

  points.str("");position.str("");
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << innerRadius <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  //points <<  -delfi1/2. << " " << delfi1/2. << endl;
  points <<  0. << " " << delfi1 << endl;

  ///////////    sector in Fi definition     /////////////////////////

  Mpdshape* cpcbox3fiu = new Mpdshape(f, "cpc01sectu", "cpc01vfl", "TUBS", "air", points.str());                       

  cpcbox3fiu->SetPosition(0.0,0.0,0.0);
  cpcbox3fiu->SetSegment(1);
  cpcbox3fiu->SetMotherSegment(1);
  cpcbox3fiu->DumpWithIncrement();

  Double_t angle = 0.0;
  for(Int_t ii = 1; ii < phipadsnumber1; ii++)
    {
      angle += delfi1;
      cpcbox3fiu->SetRotation(0.0,0.0,angle);
      cpcbox3fiu->DumpWithIncrement();
    }

 ///////////    padFi1 helpers     //////////////////////////////////

  

  points.str(""); position.str("");
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << innerRadius <<" "<< Radius1-delR1 <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;


  ///////////      padFi1 definition     /////////////////////////////

  Mpdshape* cpcbox4fi1u = new Mpdshape(f, "cpc01padR1", "cpc01sectu", "TUBS", "copper", points.str());                       

  //cpcbox4fi1u->SetPosition(0.0,0.0,0.0);
  cpcbox4fi1u->SetSegment(1);
  cpcbox4fi1u->SetMotherSegment(1);
  cpcbox4fi1u->DumpWithIncrement();
  
/*  
   ///////////      padFi2 helpers     ///////////////////////////////

  points.str(""); position.str("");
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << Radius1+delR1 <<" "<< Radius2-delR1 <<endl;
  points << 0. <<" "<< 0. <<" "<< halflayerthkns <<endl;
  points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;


  ///////////      padFi2 definition     ///////////////////////////////////////////////////
  Mpdshape* cpcbox4fi2u = new Mpdshape(f, "cpc01padR2", "cpc01sectu", "TUBS", "copper", points.str());                       

  //cpcbox4fi2u->SetPosition(0.0,0.0,0.0);
  cpcbox4fi2u->SetSegment(1);
  cpcbox4fi2u->SetMotherSegment(1);
  cpcbox4fi2u->DumpWithIncrement();
  

*/

  ///////////     padFi3 helpers - first level, part 1 ///////////////////////////////

  points.str("");position.str("");
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << Radius1+delR1 <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  //points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;
  points << 0. << " " << delfi1/2. << endl;


  ///////////     padFi3 definition - first level, part 1  ///////////////////////////

  
Mpdshape* cpcbox4fi3u1 = new Mpdshape(f, "cpc01padR31", "cpc01sectu", "TUBS", "air", points.str());  

  cpcbox4fi3u1->SetSegment(1);
  cpcbox4fi3u1->SetMotherSegment(1);
  cpcbox4fi3u1->DumpWithIncrement();


  ///////////     padFi3 helpers - first level, part 2 ///////////////////////////////

  points.str("");position.str("");
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << Radius1+delR1 <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  //points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;
  points << delfi1/2. << " " << delfi1 << endl;


  ///////////     padFi3 definition - first level, part 2  ///////////////////////////

  
Mpdshape* cpcbox4fi3u2 = new Mpdshape(f, "cpc01padR32", "cpc01sectu", "TUBS", "air", points.str());  

  cpcbox4fi3u2->SetSegment(1);
  cpcbox4fi3u2->SetMotherSegment(1);
  cpcbox4fi3u2->DumpWithIncrement();


  points.str("");position.str("");
  //points << 0. <<" "<< 0. <<" "<< 0. <<endl;
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << Radius1+delR1 <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  //points << 0. <<" "<< outerRadius-(Radius1+delR1) <<endl;
  //points << 0. <<" "<< 0. <<" "<< (halflayerthkns-halfrohcfcg10) <<endl;
  //points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;
  points << delfi1_frac*(-delfi1/4.)+delfi1/4. << " " << delfi1_frac*(+delfi1/4.)+delfi1/4. << endl;

Mpdshape* cpcbox4fi3u1_1 = new Mpdshape(f, "cpc01padR3_1", "cpc01padR31", "TUBS", "copper", points.str());  
                     
  //cpcbox4fi3u_1->SetPosition(0.0,0.0,0.0);
  cpcbox4fi3u1_1->SetSegment(1);
  cpcbox4fi3u1_1->SetMotherSegment(1);
  cpcbox4fi3u1_1->DumpWithIncrement();
 
  points.str("");position.str("");
  //points << 0. <<" "<< 0. <<" "<< 0. <<endl;
  points << 0. <<" "<< 0. <<" "<< halfrohcfcg10 <<endl;
  points << Radius1+delR1 <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< halfpanelthkns <<endl;
  //points << 0. <<" "<< outerRadius-(Radius1+delR1) <<endl;
  //points << 0. <<" "<< 0. <<" "<< (halflayerthkns-halfrohcfcg10) <<endl;
  //points << delfi1_frac*(-delfi1/2.)+delfi1/2.  << " " << delfi1_frac*(delfi1/2.)+delfi1/2. << endl;
  points << delfi1_frac*(-delfi1/4.)+3.*delfi1/4. << " " << delfi1_frac*(+delfi1/4.)+3.*delfi1/4. << endl;

Mpdshape* cpcbox4fi3u2_2 = new Mpdshape(f, "cpc01padR3_2", "cpc01padR32", "TUBS", "copper", points.str());  
                     
  //cpcbox4fi3u_2->SetPosition(0.0,0.0,0.0);
  cpcbox4fi3u2_2->SetSegment(1);
  cpcbox4fi3u2_2->SetMotherSegment(1);
  cpcbox4fi3u2_2->DumpWithIncrement();

  ///////////     sector in R helpers     ////////////////////////////

  points.str("");position.str("");
  points << 0. <<" "<< 0. <<" "<< -halfpanelthkns <<endl;
  points << innerRadius <<" "<< outerRadius <<endl;
  points << 0. <<" "<< 0. <<" "<< -halfrohcfcg10 <<endl;
  //points <<  -delfi2/2. << " " << delfi2/2. << endl;
  points <<  0. << " " << delfi2 << endl;

  ///////////     sector in R definition     /////////////////////////

  Mpdshape* cpcbox3ru = new Mpdshape(f, "cpc01sectd", "cpc01vrl", "TUBS", "air", points.str());    
                   
  //cpcbox4fi3u->SetPosition(0.0,0.0,0.0);
  cpcbox3ru->SetSegment(1);
  cpcbox3ru->SetMotherSegment(1);
  cpcbox3ru->DumpWithIncrement();

  Double_t angle2 = 0.0;
  for(Int_t ii = 1; ii < phipadsnumber2; ii++)
    {
      angle2 += delfi2;
      cpcbox3ru->SetRotation(0.0,0.0,angle2);
      cpcbox3ru->DumpWithIncrement();
    }

  Double_t  delfracfi2=delfi2_frac*(delfi2/2.);
  Double_t  delfracr2=(1.-delr2_frac)*delr2/2.;
  std::string name;

  for (Int_t ii=0; ii<rpadsnumber2; ii++)
    {
      name = "cpc01padPhi";
      name += itoa(ii+1);
      points.str(""); position.str("");
      points << 0. <<" "<< 0. <<" "<< -halfpanelthkns <<endl;
      points << innerRadius + delr2 * Double_t(ii)+delfracr2 <<" "<< innerRadius + delr2 * Double_t(ii+1)-delfracr2 <<endl;
      points << 0. <<" "<< 0. <<" "<< -halfrohcfcg10 <<endl;
      points <<  -delfracfi2+delfi2/2. << " " << delfracfi2+delfi2/2. << endl;
      position << 0. << " " << 0.0 << " " << 0.0;

      // padPhi layer definition
      Mpdshape* cpcbox4ru = new Mpdshape(f, name, "cpc01sectd", "TUBS", "copper", points.str(),position.str());    
                   
      cpcbox4ru->SetSegment(1);
      cpcbox4ru->SetMotherSegment(1);
      cpcbox4ru->DumpWithIncrement();     
    }

//outer part

// 4 G10 layers 

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfgapthkns << endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << halfgapthkns;
        Mpdshape* cpcbox2g10in = new Mpdshape(f, "cpc01g10in", "cpc01slice2", "TUBE", "G10",points.str());                       
        //cpcbox2gas->SetPosition(0.0,0.0,cpcposmflt+layerthickness);
        cpcbox2g10in->SetPosition(0.0,0.0,0.0);
        cpcbox2g10in->SetSegment(1);
        cpcbox2g10in->SetMotherSegment(1);
        cpcbox2g10in->DumpWithIncrement();

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfgapthkns << endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << halfgapthkns;
        Mpdshape* cpcbox2g10out = new Mpdshape(f, "cpc01g10out", "cpc01slice2", "TUBE", "G10",points.str());                       
        //cpcbox2gas->SetPosition(0.0,0.0,cpcposmflt+layerthickness);
        cpcbox2g10out->SetPosition(0.0,0.0,0.0);
        cpcbox2g10out->SetSegment(1);
        cpcbox2g10out->SetMotherSegment(1);
        cpcbox2g10out->DumpWithIncrement();

// 5 rohacell+carbon fiber layers 

//rohacell points                  
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -rohacellthickness/2.0 << endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << rohacellthickness/2.0;
        Mpdshape* cpcbox2rohin = new Mpdshape(f, "cpc01rohin", "cpc01slice1", "TUBE", "rohacellhf71",points.str());     
        //cpcbox2roh->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2rohin->SetPosition(0.0,0.0,0.0);
        cpcbox2rohin->SetSegment(1);
        cpcbox2rohin->SetMotherSegment(1);
        cpcbox2rohin->DumpWithIncrement();

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -rohacellthickness/2.0 << endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << rohacellthickness/2.0;
        Mpdshape* cpcbox2rohout = new Mpdshape(f, "cpc01rohout", "cpc01slice1", "TUBE", "rohacellhf71",points.str());     
        //cpcbox2roh->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2rohout->SetPosition(0.0,0.0,0.0);
        cpcbox2rohout->SetSegment(1);
        cpcbox2rohout->SetMotherSegment(1);
        cpcbox2rohout->DumpWithIncrement();

 //carbon fiber points                  
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfrohcfc<< endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << -halfrohacellthkns;
        Mpdshape* cpcbox2cfcmin = new Mpdshape(f, "cpc01cfcmin", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcm->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcmin->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcmin->SetSegment(1);
        cpcbox2cfcmin->SetMotherSegment(1);
        cpcbox2cfcmin->DumpWithIncrement();
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << halfrohacellthkns<< endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << halfrohcfc;
        Mpdshape* cpcbox2cfcpin = new Mpdshape(f, "cpc01cfcpin", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcp->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcpin->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcpin->SetSegment(1);
        cpcbox2cfcpin->SetMotherSegment(1);
        cpcbox2cfcpin->DumpWithIncrement();

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfrohcfc<< endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << -halfrohacellthkns;
        Mpdshape* cpcbox2cfcmout = new Mpdshape(f, "cpc01cfcmout", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcm->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcmout->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcmout->SetSegment(1);
        cpcbox2cfcmout->SetMotherSegment(1);
        cpcbox2cfcmout->DumpWithIncrement();
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << halfrohacellthkns<< endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << halfrohcfc;
        Mpdshape* cpcbox2cfcpout = new Mpdshape(f, "cpc01cfcpout", "cpc01slice1", "TUBE", "carbon_fiber",points.str());     
        //cpcbox2cfcp->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2cfcpout->SetPosition(0.0,0.0,0.0);
        cpcbox2cfcpout->SetSegment(1);
        cpcbox2cfcpout->SetMotherSegment(1);
        cpcbox2cfcpout->DumpWithIncrement();

 //G10 points                  
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfpanelthkns << endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << -halfrohcfc;
        Mpdshape* cpcbox2g10min = new Mpdshape(f, "cpc01g10min", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10min->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10min->SetPosition(0.0,0.0,0.0);
        cpcbox2g10min->SetSegment(1);
        cpcbox2g10min->SetMotherSegment(1);
        cpcbox2g10min->DumpWithIncrement();
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << halfrohcfc << endl;
        points << innerRadius-outerlayerwidth << " " << innerRadius << endl;
        points << 0. << " " << 0. << " " << halfpanelthkns;
        Mpdshape* cpcbox2g10pin = new Mpdshape(f, "cpc01g10pin", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10pin->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10pin->SetPosition(0.0,0.0,0.0);
        cpcbox2g10pin->SetSegment(1);
        cpcbox2g10pin->SetMotherSegment(1);
        cpcbox2g10pin->DumpWithIncrement();

        points.str("");position.str("");
        points << 0. << " " << 0. << " " << -halfpanelthkns << endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << -halfrohcfc;
        Mpdshape* cpcbox2g10mout = new Mpdshape(f, "cpc01g10mout", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10out->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10mout->SetPosition(0.0,0.0,0.0);
        cpcbox2g10mout->SetSegment(1);
        cpcbox2g10mout->SetMotherSegment(1);
        cpcbox2g10mout->DumpWithIncrement();
        points.str("");position.str("");
        points << 0. << " " << 0. << " " << halfrohcfc << endl;
        points << outerRadius << " " << outerRadius+outerlayerwidth << endl;
        points << 0. << " " << 0. << " " << halfpanelthkns;
        Mpdshape* cpcbox2g10pout = new Mpdshape(f, "cpc01g10pout", "cpc01slice1", "TUBE", "G10",points.str());     
        //cpcbox2g10pout->SetPosition(0.0,0.0,cpcposmflt);
        cpcbox2g10pout->SetPosition(0.0,0.0,0.0);
        cpcbox2g10pout->SetSegment(1);
        cpcbox2g10pout->SetMotherSegment(1);
        cpcbox2g10pout->DumpWithIncrement();

  for (Int_t i=2; i<numberoflayers; i++){
     if(i%2==0){ 
        cpcbox1->SetPosition(0.0,0.0,-fourlayerthickness+0.5*(panelthickness+gapthickness)*Double_t(i));
        cpcbox1->SetMotherSegment(1);
        cpcbox1->SetSegment(i);
        cpcbox1->DumpWithIncrement();
     }else{
        cpcbox2->SetPosition(0.0,0.0,-fourlayerthickness+0.5*(panelthickness+gapthickness)*Double_t(i));
        cpcbox2->SetMotherSegment(1);
        cpcbox2->SetSegment(i);
        cpcbox2->DumpWithIncrement();
     }
  }  
   //strcpy(cpc01ltmp,"cpc01prt");

//**************************************************************************************

  //////////// close geometry file  
  f->close();
}

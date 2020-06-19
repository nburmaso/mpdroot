void SetMode1_for_PDG(Int_t pdg, Float_t b0,Int_t m00,Int_t m01)
{
  Int_t mode[6][3];                  
  Float_t bratio[6];

  for (Int_t kz = 0; kz < 6; kz++) {
    bratio[kz] = 0.;
    mode[kz][0] = 0;
    mode[kz][1] = 0;
    mode[kz][2] = 0;
  }

  bratio[0] = b0; mode[0][0] = m00; mode[0][1] = m01;

  gMC->SetDecayMode(pdg,bratio,mode);
}

void SetMode2_for_PDG(Int_t pdg, Float_t b0,Int_t m00,Int_t m01,Int_t m02,Float_t b1,Int_t m10,Int_t m11,Int_t m12)
{
  Int_t mode[6][3];                  
  Float_t bratio[6];

  for (Int_t kz = 0; kz < 6; kz++) {
    bratio[kz] = 0.;
    mode[kz][0] = 0;
    mode[kz][1] = 0;
    mode[kz][2] = 0;
  }

  bratio[0] = b0; mode[0][0] = m00; mode[0][1] = m01; mode[0][2] = m02;
  bratio[0] = b1; mode[1][0] = m10; mode[1][1] = m11; mode[1][2] = m12;

  gMC->SetDecayMode(pdg,bratio,mode);
}

void SetMode_for_PDG(Int_t pdg, Float_t* b, Int_t* m0, Int_t* m1, Int_t *m2, Int_t N)
{
	Int_t mode[6][3];
	Float_t bratio[6];
	
	for (Int_t kz = 0; kz < 6; kz++)
	{
		bratio[kz] = 0.;
		mode[kz][0] = 0;
		mode[kz][1] = 0;
		mode[kz][2] = 0;
	}
	if (N < 7)
	{
		for (Int_t nn = 0; nn < N; nn++)
		{
			bratio[nn] = b[nn];
			mode[nn][0] = m0[nn];
			mode[nn][1] = m1[nn];
			mode[nn][2] = m2[nn];
		}
		for (Int_t nn = 0; nn < 6; nn++)
		gMC->SetDecayMode(pdg,bratio,mode);
	}
} 

void UserDecayConfig()
{
	const Double_t kAu2Gev=0.9314943228;
	const Double_t khSlash = 1.0545726663e-27;
	const Double_t kErg2Gev = 1/1.6021773349e-3;
	const Double_t khShGev = khSlash*kErg2Gev;
	const Double_t kYear2Sec = 3600*24*365.25;
	cout << "Loading User Decay Config from macro"<< endl;  
	TDatabasePDG *db= TDatabasePDG::Instance();
	TParticlePDG *p=0;
	
	Int_t m1[6], m2[6], m3[6], ip;
	Float_t b[6], totB;
	Int_t AlphaPDG, He5PDG, He3, H3L, H4L, prot, deut, neut, H3, He4L;
	
	p = db->GetParticle("proton");
	if (p) prot = p->PdgCode();
	
	p = db->GetParticle("Deuteron");
	if (p) deut = p->PdgCode();
	else { deut = 1000010020; db->AddParticle("Deuteron","Deuteron",2*kAu2Gev+8.071e-3,kTRUE,0,3,"Ion",deut); }
	
	p = db->GetParticle("neutron");
	if (p) neut = p->PdgCode();
	
	p = db->GetParticle("Triton");
	if (p) H3 = p->PdgCode();
	else { H3 = 1000010030; db->AddParticle("Triton","Triton",3*kAu2Gev+14.931e-3,kFALSE,khShGev/(12.33*kYear2Sec),3,"Ion",H3); }
	
	p = db->GetParticle("He4L");
	if (p) He4L = p->PdgCode();
	else { He4L = 1010020040; db->AddParticle("He4L","He4L",3.92501,kFALSE,khShGev/(12.33*kYear2Sec),6,"Ion",He4L); 
		gMC->DefineParticle(He4L, "He4L", kPTHadron, 3.92501 , 2.0, 2.632e-10,"Ion", 0.0, 0, 1, 0, 0, 0, 0, 0, 4, kFALSE);}
	
	p = db->GetParticle("Alpha");
	if (!p) {
		p = db->GetParticle("alpha");
		if (!p) {
			p = db->GetParticle("He4");
				}
			}
	
	if(p) AlphaPDG = p->PdgCode();
	else { AlphaPDG = 1000020040; db->AddParticle("Alpha","Alpha",4*kAu2Gev+2.424e-3,kTRUE,khShGev/(12.33*kYear2Sec),6,"Ion",AlphaPDG); }
	
	cout << "!!!! 0-0 !!!!!!! " <<  (db->ParticleList()->GetEntries()) << " " << AlphaPDG << endl;
	
	p = db->GetParticle("He3");
	if (!p) p = db->GetParticle("HE3");
	if (p) He3 = p->PdgCode();
	else { He3=1000020030; db->AddParticle("HE3","HE3", 2.80923,kFALSE,0,6,"Ion",He3); }
	
	cout << "!!!! 0-1 !!!!!!! " <<  (db->ParticleList()->GetEntries()) << " " << He3 << endl;
	
	if (He3)
	{
		p = db->GetParticle("H3L");
		if (p) H3L = p->PdgCode();
		else { H3L = 1010010030; gMC->DefineParticle(H3L, "H3L", kPTHadron, 2.99131 , 1.0, 2.632e-10,"Ion", 0.0, 0, 1, 0, 0, 0, 0, 0, 3, kFALSE); }
		
		// ----- H3L decays -----
		//SetMode1_for_PDG(H3L,100,-211,He3);
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 24.7; m1[ip] = -211; m2[ip] = He3; m3[ip] = 0; // pi-, He3
		b[++ip] = 12.35; m1[ip] = 111; m2[ip] = H3; m3[ip] = 0; // pi0, H3
		b[++ip] = 36.7; m1[ip] = -211; m2[ip] = prot; m3[ip] = deut; // pi-, p, d
		b[++ip] = 18.35; m1[ip] = 111; m2[ip] = neut; m3[ip] = deut; // pi0, n, d
		b[++ip] = 0.2; m1[ip] = neut; m2[ip] = deut; m3[ip] = 0; // n, d
		b[++ip] = 1.5; m1[ip] = neut; m2[ip] = neut; m3[ip] = prot; // n, n, p
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(H3L,b,m1,m2,m3,++ip);
		
		p = db->GetParticle("H4L");
		if (p) H4L = p->PdgCode();
		else { H4L = 1010010040; gMC->DefineParticle(H4L, "H4L", kPTHadron, 3.92503 , 1.0, 2.632e-10,"Ion", 0.0, 0, 1, 0, 0, 0, 0, 0, 4, kFALSE); }
		
		// ----- H4L decays -----
		//SetMode1_for_PDG(H4L,100,-211,AlphaPDG);
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 75.; m1[ip] = -211; m2[ip] = AlphaPDG; m3[ip] = 0; // pi-, He4
		b[++ip] = 25.; m1[ip] = H3; m2[ip] = prot; m3[ip] = -211; // H3, prot, pi-
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(H4L,b,m1,m2,m3,++ip);
	}
	
	// ----- He4L decays -----
	for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
	ip = -1; totB = 0.;
	b[++ip] = 32.; m1[ip] = -211; m2[ip] = He3; m3[ip] = prot; // pi-, He3, p
	b[++ip] = 35.; m1[ip] = 111; m2[ip] = AlphaPDG; m3[ip] = 0; // pi0, He4
	b[++ip] = 14.; m1[ip] = 111; m2[ip] = He3; m3[ip] = neut; // pi0, He3, neut
	b[++ip] = 19.; m1[ip] = prot; m2[ip] = H3; m3[ip] = 0; // p, H3
	
	for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
	for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
	SetMode_for_PDG(He4L,b,m1,m2,m3,++ip);
	
	p = db->GetParticle("LN");
	if (!p) {
		p = db->GetParticle("LambdaNeutron");
		if (!p) {
			gMC->DefineParticle(1010000020, "LN", kPTNeutron, 2.054 , 0.0, 2.632e-10,"Ion", 0.0, 0, 1, 0, 0, 0, 0, 0, 2, kFALSE);
			p = db->GetParticle("LN");
				}
			}
	if (p) SetMode1_for_PDG(p->PdgCode(),100,1000010020,-211);
	
	cout << "!!!! 1-0 !!!!!!! " <<  (db->ParticleList()->GetEntries()) << " " << H3L<< " " << H4L << endl;

        cout<<"====== Redefinition of decay channels for eta/rho/omega/phi/eta-prime ======"<<endl;

		// ----- eta -----
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 39.40; m1[ip] =  22; m2[ip] =   22; m3[ip] =   0; // (gamma, gamma) + 0.08
		b[++ip] = 32.68; m1[ip] = 111; m2[ip] =  111; m3[ip] = 111; // pi0, pi0, pi0
		b[++ip] = 22.92; m1[ip] = 211; m2[ip] = -211; m3[ip] = 111; // pi+, pi-, pi0
		b[++ip] =  4.22; m1[ip] = 211; m2[ip] = -211; m3[ip] =  22; // pi+, pi-, gamma
		b[++ip] =  0.69; m1[ip] =  11; m2[ip] =  -11; m3[ip] =  22; // e-, e+, gamma
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(221,b,m1,m2,m3,++ip);


		// ----- rho0 -----
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 99.9056; m1[ip] = -211; m2[ip] = 211; m3[ip] = 0; // pi-, pi+
		b[++ip] =  0.0944; m1[ip] =   11; m2[ip] = -11; m3[ip] = 0; // (e-, e+) x 20
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(113,b,m1,m2,m3,++ip);


		// ----- omega (pid=223) -----
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 88.3828; m1[ip] = 211; m2[ip] = -211; m3[ip] = 111; // (pi+, pi-, pi0) - 0.9172
		b[++ip] =  8.400;  m1[ip] = 111; m2[ip] =   22; m3[ip] =   0; // pi0, gamma
		b[++ip] =  1.530;  m1[ip] = 211; m2[ip] = -211; m3[ip] =   0; // pi+, pi-
		b[++ip] =  1.540;  m1[ip] = 111; m2[ip] =  -11; m3[ip] =  11; // (pi0, e+, e-) x 20
		b[++ip] =  0.1472; m1[ip] =  11; m2[ip] =  -11; m3[ip] =   0; // (e-, e+)  x 20
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(223,b,m1,m2,m3,++ip);


		// ----- phi (pid=333) -----
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 49.8234; m1[ip] = 321; m2[ip] = -321; m3[ip] =   0; // (K+, K-) + 0.6234
		b[++ip] = 34.000;  m1[ip] = 130; m2[ip] =  310; m3[ip] =   0; // Kl0, Ks0
		b[++ip] = 15.240;  m1[ip] = 211; m2[ip] = -211; m3[ip] = 111; // pi+, pi-, pi0
		b[++ip] =  0.130;  m1[ip] = 221; m2[ip] =   22; m3[ip] =   0; // eta, gamma
		b[++ip] =  0.5946; m1[ip] =  11; m2[ip] =  -11; m3[ip] =   0; // (e-, e+) x 20 
		b[++ip] =  0.212;  m1[ip] = 221; m2[ip] =  -11; m3[ip] =  11; // (eta, e+, e-) x 20
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(333,b,m1,m2,m3,++ip);



		// ----- eta-prime (pid=331) -----
		for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
		ip = -1; totB = 0.;
		b[++ip] = 42.514; m1[ip] = 221; m2[ip] = -211; m3[ip] = 211; // (eta, pi+, pi-)  - 0.086
		b[++ip] = 28.90;  m1[ip] = 113; m2[ip] =   22; m3[ip] =   0; // rho0, gamma
		b[++ip] = 22.80;  m1[ip] = 221; m2[ip] =  111; m3[ip] = 111; // eta, pi0, pi0
		b[++ip] =  2.22;  m1[ip] =  22; m2[ip] =   22; m3[ip] =   0; // gamma, gamma
		b[++ip] =  2.62;  m1[ip] = 223; m2[ip] =   22; m3[ip] =   0; // (omega, gamma)
		b[++ip] =  0.946; m1[ip] =  22; m2[ip] =  -11; m3[ip] =  11; // (gamma, e+, e-) x 20
		
		for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
		for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;
		
		SetMode_for_PDG(331,b,m1,m2,m3,++ip);


}


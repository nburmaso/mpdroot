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
	
//V
	//Lambda(1520)
	gMC->DefineParticle(3124, "LAMS", kPTNeutron, 1.5195 , 0.0, 4.219231e-23,"Ion", 0.0156, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added Lambda(1520)"<<endl;

	//anti-Lambda(1520)
	gMC->DefineParticle(-3124, "ALAMS", kPTNeutron, 1.5195 , 0.0, 4.219231e-23,"Ion", 0.0156, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-Lambda(1520)"<<endl;

	//K*(892)0
	gMC->DefineParticle(313, "KST0", kPTNeutron, 0.89555 , 0.0, 1.39154e-23,"Ion", 0.0473, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added K*(892)0"<<endl;

	//anti-K*(892)0
	gMC->DefineParticle(-313, "AKST0", kPTNeutron, 0.89555 , 0.0, 1.39154e-23,"Ion", 0.0473, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-K*(892)0"<<endl;

	//K*(892)+
	gMC->DefineParticle(323, "KSTP", kPTHadron, 0.89166 , 1.0, 1.308548e-23,"Ion", 0.0508, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added K*(892)+"<<endl;

	//K*(892)-
	gMC->DefineParticle(-323, "KSTM", kPTHadron, 0.89166 , -1.0, 1.308548e-23,"Ion", 0.0508, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added K*(892)-"<<endl;

	//Sigma(1385)+
	gMC->DefineParticle(3224, "SIGP", kPTHadron, 1.38280 , 1.0, 1.84e-23,"Ion", 0.036, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added Sigma(1385)+"<<endl;

	//Sigma(1385)-
	gMC->DefineParticle(3114, "SIGM", kPTHadron, 1.38720 , -1.0, 1.67e-23,"Ion", 0.0394, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added Sigma(1385)-"<<endl;

	//anti-Sigma(1385)+
	gMC->DefineParticle(-3224, "ASIGP", kPTHadron, 1.38280 , -1.0, 1.84e-23,"Ion", 0.036, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-Sigma(1385)+"<<endl;

	//anti-Sigma(1385)-
	gMC->DefineParticle(-3114, "ASIGM", kPTHadron, 1.38720 , 1.0, 1.67e-23,"Ion", 0.0394, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-Sigma(1385)-"<<endl;

	//Xi(1530)
	gMC->DefineParticle(3324, "XIST", kPTNeutron, 1.53180 , 0.0, 7.28e-23,"Ion", 0.0091, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added Xi(1530)"<<endl;

	//anti-Xi(1530)
	gMC->DefineParticle(-3324, "AXIST", kPTNeutron, 1.53180 , 0.0, 7.28e-23,"Ion", 0.0091, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-Xi(1530)"<<endl;

	//Sigma(1385)0
	gMC->DefineParticle(3214, "SIG0", kPTNeutron, 1.3837 , 0.0, 1.84e-23,"Ion", 0.036, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added Sigma(1385)0"<<endl;

	//anti-Sigma(1385)0
	gMC->DefineParticle(-3214, "ASIG0", kPTNeutron, 1.3837 , 0.0, 1.84e-23,"Ion", 0.036, 0, 0, 0, 0, 0, 0, 0, 0, kFALSE);
	cout<<"Added anti-Sigma(1385)0"<<endl;
//V

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

//V
cout<<"===== (re)defined decays for (a)-L*, (a)-K*0, K*+/-, (a)-Sigma(1385)+, (a)-Sigma(1385)-, (a)-Xi(1530) ====="<<endl;
                // ----- LAMS (pid=3124) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] = 22.50;  m1[ip] =  2212; m2[ip] = -321; m3[ip] =   0; // p, K-
                b[++ip] = 22.50;  m1[ip] = -2212; m2[ip] =  321; m3[ip] =   0; // antip, K+
                b[++ip] = 45.00;  m1[ip] =  3212; m2[ip] =  111; m3[ip] =   0; // Sigma0, pi0
                b[++ip] = 10.00;  m1[ip] =  3122; m2[ip] =  111; m3[ip] = 111; // Lambda, pi0, pi0

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(3124,b,m1,m2,m3,++ip);


                // ----- ALAMS (pid=-3124) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] = 22.50;  m1[ip] = -2212; m2[ip] =  321; m3[ip] =   0; // antip, K+
                b[++ip] = 22.50;  m1[ip] =  2212; m2[ip] = -321; m3[ip] =   0; // p, K-
                b[++ip] = 45.00;  m1[ip] = -3212; m2[ip] =  111; m3[ip] =   0; // anti-Sigma0, pi0
                b[++ip] = 10.00;  m1[ip] = -3122; m2[ip] =  111; m3[ip] = 111; // anti-Lambda, pi0, pi0

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-3124,b,m1,m2,m3,++ip);


                // ----- KST0 (pid=313) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  66.50;  m1[ip] = 321; m2[ip] =  -211; m3[ip] =   0; // K+, pi-
                b[++ip] =  16.65;  m1[ip] = 130; m2[ip] =   111; m3[ip] =   0; // KL, pi0
                b[++ip] =  16.65;  m1[ip] = 310; m2[ip] =   111; m3[ip] =   0; // Ks, pi0
                b[++ip] =  0.10;   m1[ip] = 130; m2[ip] =    22; m3[ip] =   0; // KL, gamma
                b[++ip] =  0.10;   m1[ip] = 310; m2[ip] =    22; m3[ip] =   0; // Ks, gamma

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(313,b,m1,m2,m3,++ip);


                // ----- AKST0 (pid=-313) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  66.50;  m1[ip] = -321; m2[ip] =   211; m3[ip] =   0; // K-, pi+
                b[++ip] =  16.65;  m1[ip] =  130; m2[ip] =   111; m3[ip] =   0; // KL, pi0
                b[++ip] =  16.65;  m1[ip] =  310; m2[ip] =   111; m3[ip] =   0; // Ks, pi0
                b[++ip] =  0.10;   m1[ip] = 130; m2[ip] =    22;  m3[ip] =   0; // KL, gamma
                b[++ip] =  0.10;   m1[ip] = 310; m2[ip] =    22;  m3[ip] =   0; // Ks, gamma

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-313,b,m1,m2,m3,++ip);


                // ----- KSTP (pid=323) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  33.30;  m1[ip] =  130; m2[ip] =   211; m3[ip] =   0; // KL, pi+
                b[++ip] =  33.30;  m1[ip] =  310; m2[ip] =   211; m3[ip] =   0; // Ks, pi+
                b[++ip] =  33.30;  m1[ip] =  321; m2[ip] =   111; m3[ip] =   0; // K+, pi0
                b[++ip] =   0.10;  m1[ip] =  321; m2[ip] =    22; m3[ip] =   0; // K+, gamma

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(323,b,m1,m2,m3,++ip);


                // ----- KSTM (pid=-323) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  33.30;  m1[ip] =  130; m2[ip] =  -211; m3[ip] =   0; // KL, pi-
                b[++ip] =  33.30;  m1[ip] =  310; m2[ip] =  -211; m3[ip] =   0; // Ks, pi-
                b[++ip] =  33.30;  m1[ip] = -321; m2[ip] =   111; m3[ip] =   0; // K-, pi0
                b[++ip] =   0.10;  m1[ip] = -321; m2[ip] =    22; m3[ip] =   0; // K-, gamma

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-323,b,m1,m2,m3,++ip);


                // ----- SIGP (pid=3224) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = 3122; m2[ip] =  211; m3[ip] =   0; // Lambda, pi+
                b[++ip] =   6.00;  m1[ip] = 3222; m2[ip] =  111; m3[ip] =   0; // Sigma+, pi0
                b[++ip] =   6.00;  m1[ip] = 3212; m2[ip] =  211; m3[ip] =   0; // Sigma0, pi+

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(3224,b,m1,m2,m3,++ip);


                // ----- ASIGP (pid=-3224) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = -3122; m2[ip] = -211; m3[ip] =   0; // anti-Lambda, pi-
                b[++ip] =   6.00;  m1[ip] = -3222; m2[ip] =  111; m3[ip] =   0; // anti-Sigma+, pi0
                b[++ip] =   6.00;  m1[ip] = -3212; m2[ip] = -211; m3[ip] =   0; // anti-Sigma0, pi-

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-3224,b,m1,m2,m3,++ip);


                // ----- SIGM (pid=3114) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = 3122; m2[ip] = -211; m3[ip] =   0; // Lambda, pi-
                b[++ip] =   6.00;  m1[ip] = 3212; m2[ip] = -211; m3[ip] =   0; // Sigma0, pi-
                b[++ip] =   6.00;  m1[ip] = 3112; m2[ip] =  111; m3[ip] =   0; // Sigma-, pi0

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(3114,b,m1,m2,m3,++ip);


                // ----- ASIGM (pid=-3114) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = -3122; m2[ip] = 211; m3[ip] =   0; // anti-Lambda, pi+
                b[++ip] =   6.00;  m1[ip] = -3212; m2[ip] = 211; m3[ip] =   0; // anti-Sigma0, pi+
                b[++ip] =   6.00;  m1[ip] = -3112; m2[ip] = 111; m3[ip] =   0; // anti-Sigma-, pi0

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-3114,b,m1,m2,m3,++ip);


                // ----- XIST (pid=3324) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  33.30;  m1[ip] = 3322; m2[ip] = 111; m3[ip] =   0; // Xi0, pi0
                b[++ip] =  66.70;  m1[ip] = 3312; m2[ip] = 211; m3[ip] =   0; // Xi-, pi+

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(3324,b,m1,m2,m3,++ip);


                // ----- AXIST (pid=-3324) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  33.30;  m1[ip] = -3322; m2[ip] =  111; m3[ip] =   0; // anti-Xi0, pi0
                b[++ip] =  66.70;  m1[ip] = -3312; m2[ip] = -211; m3[ip] =   0; // anti-Xi-, pi-

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-3324,b,m1,m2,m3,++ip);


                // ----- SIG0 (pid=3214) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = 3122; m2[ip] =  111; m3[ip] =   0; // Lambda, pi0
                b[++ip] =   6.00;  m1[ip] = 3222; m2[ip] = -211; m3[ip] =   0; // Sigma+, pi-
                b[++ip] =   6.00;  m1[ip] = 3112; m2[ip] =  211; m3[ip] =   0; // Sigma-, pi+

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(3214,b,m1,m2,m3,++ip);


                // ----- ASIG0 (pid=-3214) -----
                for (Int_t kz=0; kz<6; kz++) { b[kz] = 0.; m1[kz] = 0; m2[kz] = 0; m3[kz] = 0; }
                ip = -1; totB = 0.;
                b[++ip] =  88.00;  m1[ip] = -3122; m2[ip] =  111; m3[ip] =   0; // Lambda, pi0
                b[++ip] =   6.00;  m1[ip] = -3222; m2[ip] =  211; m3[ip] =   0; // Sigma+, pi-
                b[++ip] =   6.00;  m1[ip] = -3112; m2[ip] = -211; m3[ip] =   0; // Sigma-, pi+

                for (Int_t kz=0; kz<ip+1; kz++) totB += b[kz] / 100.;
                for (Int_t kz=0; kz<ip+1; kz++) b[kz] /= totB;

                SetMode_for_PDG(-3214,b,m1,m2,m3,++ip);
//V

}


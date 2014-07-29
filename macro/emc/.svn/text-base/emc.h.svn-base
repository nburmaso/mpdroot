  Double_t z = 0.0;


  Double_t emc1_box1_h = 400.00;
  Double_t box_pb_h = 0.350;			
  Double_t box_sc_h = 1.5;
  Double_t base = 40.;
  Double_t box_sum = box_pb_h+box_sc_h;
  
  Double_t inr = 1798.;
  Double_t outr = inr+emc1_box1_h;
  Double_t emc1Chamber_z_l = 5900.;
    
  Int_t number_emc1Chamber = 115;
  Double_t r = 360./number_emc1Chamber;

  Double_t outro = outr*TMath::Cos(TMath::Pi()/number_emc1Chamber);
  emc1_box1_h = outro-inr;
  Int_t st = emc1_box1_h/box_sum;
  Double_t emc1_box1_h_h = st*box_sum;

  Double_t emc1_box1_u =base*3;    

  Double_t emc1_box1_d= (TMath::Pi()*2*inr)/number_emc1Chamber;      
  Double_t box_s_d =(emc1_box1_d-base)/2;
  Double_t box_s_u = (emc1_box1_u-base)/2;
  Double_t b_box = (emc1_box1_u-emc1_box1_d)/2;

  Double_t q = TMath::ATan(b_box/emc1_box1_h);

  Double_t tr_b = TMath::Tan(q)*emc1_box1_h/2.;
  Double_t half_a = box_s_d+tr_b;
  
  Double_t alfa = q*180/TMath::Pi();
  Double_t beta = 90. - alfa;
  Double_t qb = TMath::DegToRad()*beta;

  Double_t bpb = box_pb_h;
  Double_t bsc = box_sc_h;
  Double_t apb = bpb*TMath::Tan(q);
  Double_t asc = bsc*TMath::Tan(q);

  Double_t emc1Chamber_z;
  Int_t nbox1_inl = emc1Chamber_z_l/emc1_box1_u;
  emc1Chamber_z = nbox1_inl*emc1_box1_u;
 
  Double_t emc1Chamber_z_th = emc1Chamber_z/2 - emc1_box1_u/2;
  

  Double_t ad = box_s_d;
  Double_t draw_st_pos=90.;

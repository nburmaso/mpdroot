void DecayConfig()
{
  // External decayer configuration file

  MpdDecayer* decayer = MpdDecayer::Instance();
  gMC->SetExternalDecayer(decayer);
  //gMC->SetUserDecay(3122); // user decay of Lambda-hyperon
  decayer->AddMotherPdg(3122); // user decay of Lambda-hyperon
}

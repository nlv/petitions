{ tls = 
    { crtFile = "crt.txt" 
    , keyFile = "private-key.txt"
    }
, server =
    { url = "https://petitions.nika.news"
    , port = 8080
    }
, dbconnect = "dbname=petitions user=nlv" 
}

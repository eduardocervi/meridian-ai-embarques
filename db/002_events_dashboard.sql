-- MeridIAn Comex V5.0: histórico imutável de eventos e cargas operacionais
CREATE TABLE IF NOT EXISTS events (
  event_id TEXT PRIMARY KEY,
  event_timestamp TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  operation_date DATE,
  event_type TEXT NOT NULL,
  process_number TEXT,
  client_name TEXT,
  unit_name TEXT,
  product_name TEXT,
  urf_name TEXT,
  invoice TEXT,
  crt TEXT,
  mic_dta TEXT,
  load_delta INTEGER NOT NULL DEFAULT 0,
  weight_delta NUMERIC NOT NULL DEFAULT 0,
  user_name TEXT,
  source_file_name TEXT,
  field_name TEXT,
  old_value TEXT,
  new_value TEXT,
  description TEXT
);
CREATE INDEX IF NOT EXISTS idx_events_timestamp ON events(event_timestamp);
CREATE INDEX IF NOT EXISTS idx_events_process ON events(process_number);
CREATE INDEX IF NOT EXISTS idx_events_client ON events(client_name);
CREATE INDEX IF NOT EXISTS idx_events_type ON events(event_type);

CREATE TABLE IF NOT EXISTS operational_loads (
  load_key TEXT PRIMARY KEY,
  process_number TEXT,
  mic_dta TEXT,
  net_weight NUMERIC,
  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
  source_file_name TEXT
);

CREATE TABLE IF NOT EXISTS clients (
  client_id BIGSERIAL PRIMARY KEY, client_name TEXT NOT NULL UNIQUE, active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS units (
  unit_id BIGSERIAL PRIMARY KEY, client_id BIGINT NOT NULL REFERENCES clients(client_id), unit_name TEXT NOT NULL,
  cnpj TEXT, cnpj_normalized TEXT, city TEXT, state TEXT, ie TEXT, active BOOLEAN NOT NULL DEFAULT TRUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(), updated_at TIMESTAMPTZ NOT NULL DEFAULT now()
);
CREATE TABLE IF NOT EXISTS processes (
  process_id BIGSERIAL PRIMARY KEY, business_key TEXT NOT NULL UNIQUE, client_id BIGINT REFERENCES clients(client_id), unit_id BIGINT REFERENCES units(unit_id),
  process_number TEXT, invoice TEXT, di TEXT, crt TEXT, importer_raw TEXT, exporter TEXT, product_name TEXT, urf TEXT, carrier TEXT,
  registration_date DATE, release_date DATE, expected_weight NUMERIC, shipped_weight NUMERIC, balance_weight NUMERIC,
  load_count INTEGER NOT NULL DEFAULT 0, status TEXT NOT NULL DEFAULT 'EM ABERTO', source_file_name TEXT, source_file_hash TEXT,
  first_imported_at TIMESTAMPTZ NOT NULL DEFAULT now(), last_imported_at TIMESTAMPTZ NOT NULL DEFAULT now(), last_changed_at TIMESTAMPTZ NOT NULL DEFAULT now(), active BOOLEAN NOT NULL DEFAULT TRUE
);
CREATE TABLE IF NOT EXISTS loads (
  load_id BIGSERIAL PRIMARY KEY, process_id BIGINT NOT NULL REFERENCES processes(process_id), business_key TEXT NOT NULL UNIQUE,
  issue_date DATE, mic_dta TEXT, tractor_plate TEXT, trailer_plate TEXT, net_weight NUMERIC, value NUMERIC, sequence TEXT, invoice_number TEXT,
  first_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(), last_seen_at TIMESTAMPTZ NOT NULL DEFAULT now(), source_row INTEGER, source_fingerprint TEXT, active BOOLEAN NOT NULL DEFAULT TRUE
);
CREATE TABLE IF NOT EXISTS import_batches (
  batch_id BIGSERIAL PRIMARY KEY, started_at TIMESTAMPTZ NOT NULL DEFAULT now(), finished_at TIMESTAMPTZ, user_id TEXT, source_type TEXT,
  files_found INTEGER DEFAULT 0, files_processed INTEGER DEFAULT 0, files_skipped INTEGER DEFAULT 0, files_error INTEGER DEFAULT 0,
  new_processes INTEGER DEFAULT 0, updated_processes INTEGER DEFAULT 0, new_loads INTEGER DEFAULT 0, updated_loads INTEGER DEFAULT 0, removed_loads INTEGER DEFAULT 0,
  total_weight_delta NUMERIC DEFAULT 0, status TEXT
);
CREATE TABLE IF NOT EXISTS events (
  event_id BIGSERIAL PRIMARY KEY, batch_id BIGINT REFERENCES import_batches(batch_id), event_timestamp TIMESTAMPTZ NOT NULL DEFAULT now(),
  operation_date DATE, import_date DATE, event_type TEXT NOT NULL, process_id BIGINT REFERENCES processes(process_id), load_id BIGINT REFERENCES loads(load_id),
  user_id TEXT, field_name TEXT, old_value TEXT, new_value TEXT, load_delta INTEGER, weight_delta NUMERIC,
  source_file_name TEXT, source_file_hash TEXT, description TEXT
);
CREATE INDEX IF NOT EXISTS idx_processes_lookup ON processes(process_number, invoice, di, crt, status);
CREATE INDEX IF NOT EXISTS idx_loads_lookup ON loads(mic_dta, issue_date);
CREATE INDEX IF NOT EXISTS idx_events_timestamp ON events(event_timestamp);

DROP TABLE IF EXISTS storm;
DROP USER IF EXISTS data_rw;
DROP GROUP IF EXISTS safe_mode;

CREATE TABLE storm (
    fid INTEGER,
    ts TIMESTAMP WITH TIME ZONE,
    btid INTEGER,
    name VARCHAR(255),
    lat REAL,
    long REAL,
    wind_kts SMALLINT,
    pressure SMALLINT,
    category VARCHAR(2),
    basin VARCHAR(255),
    shape_length REAL,
    UNIQUE(fid)
);

-- Create group without DELETE privilege
CREATE GROUP safe_mode NOLOGIN;
REVOKE DELETE ON ALL TABLES IN SCHEMA public FROM safe_mode;

-- Create user in safe_mode group
CREATE USER data_rw WITH PASSWORD 'password123';
REVOKE ALL ON ALL TABLES IN SCHEMA public FROM data_rw;
GRANT safe_mode TO data_rw;

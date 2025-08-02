-- SQLite
INSERT INTO users
  (id, first_name, last_name, email, active, role, createdAt, updatedAt)
VALUES
  ('897bd86c-feda-4c17-ab09-20959550899b', 'Admin', 'Adminovic', 'admin.adminovic@company.com', true, 'Admin', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
  ('064615be-15ad-4e10-b06a-6cdc46fa8788', 'Joe', 'Doe', 'joe.doe@company.com', true, 'User', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

INSERT INTO scripts
  (id, type, script, category, title, description, createdAt, updatedAt)
VALUES
  ('63a080e2-dc50-4771-a145-93ef6ec24cbe', 'PlutusV2', '49480100002221200101', 'Gift', 'Gift', 'Everybody can spend this..', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP),
  ('e59382ba-3733-4257-9ce5-43358dd73b73', 'PlutusV2', '581f581d01000022232632498cd5ce24810b6974206275726e7321212100120011', 'Burn', 'Burn', 'Nobody can spend this..', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

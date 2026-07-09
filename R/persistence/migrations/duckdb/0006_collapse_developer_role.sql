-- Collapse the 'developer' role into 'admin'. The developer role was never
-- gated anywhere in the UI (only admin vs everyone-else was enforced), and
-- the approved target role model is user/admin only. Existing developer
-- accounts become admins (the safe merge target).

UPDATE users SET role = 'admin' WHERE lower(trim(role)) IN ('dev', 'developer');

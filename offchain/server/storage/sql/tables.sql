-- SQLite
CREATE TABLE IF NOT EXISTS users (
    `id` UUID PRIMARY KEY NOT NULL,
    `first_name` VARCHAR(50) NOT NULL,
    `last_name` VARCHAR(50) NOT NULL,
    `email` VARCHAR(50) NOT NULL,
    `active` TINYINT(1) NOT NULL DEFAULT 1,
    `role` VARCHAR(10) NOT NULL DEFAULT 'User' check (role in ('Admin', 'User')),
    `createdAt` DATETIME NOT NULL,
    `updatedAt` DATETIME NOT NULL,
    constraint uq_users_email UNIQUE (email)
);

CREATE TABLE IF NOT EXISTS scripts (
    `id` UUID PRIMARY KEY NOT NULL,
    `type` VARCHAR(10) NOT NULL DEFAULT 'PlutusV2' check (type in ('PlutusV1', 'PlutusV2', 'PlutusV3', 'Native')),
    `script` VARCHAR NOT NULL,
    `category` VARCHAR(15) NOT NULL check (category in ('Burn', 'Gift', 'FortyTwo', 'Vesting', 'Unknown')),
    `title` TINYINT(25) NOT NULL,
    `description` TINYINT(100),
    `createdAt` DATETIME NOT NULL,
    `updatedAt` DATETIME NOT NULL,
    constraint uq_scripts_script UNIQUE (script)
);

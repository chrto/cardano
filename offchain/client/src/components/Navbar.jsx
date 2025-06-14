// src/components/Navbar.jsx
import { NavLink } from 'react-router-dom';
import './Navbar.css';

export default function Navbar() {
  return (
    <nav className="navbar">
      <div className="navbar-container">
        {/* <NavLink to="/" className="logo">
          Home
        </NavLink> */}
        <div className="nav-links">
          <NavLink to="/" className="nav-link" activeClassName="active" end>
            Home
          </NavLink>
          <NavLink to="/about" className="nav-link" activeClassName="active">
            About
          </NavLink>
        </div>
        <div className="nav-links">
          <NavLink to="/gift" className="nav-link" activeClassName="active">
            Gift
          </NavLink>
        </div>
      </div>
    </nav>
  );
}

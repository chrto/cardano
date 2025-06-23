// src/components/Navbar.jsx
import { NavLink } from 'react-router-dom';
import './Navbar.css';

export default function Navbar() {
  return (
    <nav className="navbar">
      <div className="navbar-container">
        <div className="nav-links">
          <NavLink to="/" className="nav-link" >Home</NavLink>
          <NavLink to="/about" className="nav-link" >About</NavLink>
        </div>
        <div className="nav-links">
          <NavLink to="/gift" className="nav-link" >Gift</NavLink>
          <NavLink to="/fortyTwo" className="nav-link" >FortyTwo</NavLink>
        </div>
      </div>
    </nav>
  );
}

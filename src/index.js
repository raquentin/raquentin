import React from 'react';
import { createRoot } from 'react-dom/client';

import Root from './App';
import './common/global.css'

const container = document.getElementById('root');
const root = createRoot(container);

root.render(
  <Root />
);
import { Outlet, useLocation } from 'react-router-dom';
import { motion } from 'framer-motion';

import PageContainer from './PageContainer';

export default function AnimationLayout() {
  const { pathname } = useLocation();

  const pageVariants = {
    initial: {
      opacity: 0
    },
    in: {
      opacity: 1
    },
    out: {
      opacity: 0
    }
  };
  
  const pageTransition = {
    type: "tween",
    ease: "easeInOut",
    duration: 0.3
  };

  return (
    <motion.div
      key={pathname}
      initial="initial"
      animate="in"
      variants={pageVariants}
      transition={pageTransition}
    >
      <PageContainer pathname={pathname}>
        <Outlet />
      </PageContainer>
    </motion.div>
  );
};
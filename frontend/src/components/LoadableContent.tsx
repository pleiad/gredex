import React from "react";
import { CircularProgress, Box } from "@mui/material";

interface LoadableContentProps {
  loading?: boolean;
  children?: React.ReactNode;
}

const LoadableContent: React.FC<LoadableContentProps> = ({
  loading = false,
  children,
}) => {
  if (loading) {
    return (
      <Box sx={{ position: "relative", textAlign: "center" }}>
        <CircularProgress
          size={50}
          sx={{ display: "inline-block", position: "relative" }}
        />
      </Box>
    );
  }

  return <>{children}</>;
};

export default LoadableContent;

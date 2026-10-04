
module soc_adapter (
  input logic clock,
  input logic reset,

  // apb
  input logic psel,
  input logic penable,
  input logic [31:0] paddr,
  input logic pwrite,
  input logic [31:0] pwdata,
  output logic [31:0] prdata,
  output logic pready,
  output logic pslverr,

  output logic uart0_ctrl_en,
  output logic uart0_ctrl_loopback,
  input logic uart0_status_txEmpty,
  input logic uart0_status_rxReady,
  output logic [7:0] uart0_data_txData,
  output logic uart0_data_txData_trg,
  input logic [7:0] uart0_data_rxData,
  output logic uart0_data_rxData_trg,
  output logic gpio0_ctrl_en,
  output logic [31:0] gpio0_dir,
  input logic [31:0] gpio0_dataIn,
  output logic [31:0] gpio0_dataOut,
  output logic gpio1_ctrl_en,
  output logic [31:0] gpio1_dir,
  input logic [31:0] gpio1_dataIn,
  output logic [31:0] gpio1_dataOut
);

  // Apb state
  reg wr_access;
  reg rd_access;
  assign pready = wr_access || rd_access;

  // Internal registers
  reg uart0_ctrl_en_reg;
  reg uart0_ctrl_loopback_reg;
  reg [7:0] uart0_data_txData_reg;
  reg gpio0_ctrl_en_reg;
  reg [31:0] gpio0_dir_reg;
  reg [31:0] gpio0_dataOut_reg;
  reg gpio1_ctrl_en_reg;
  reg [31:0] gpio1_dir_reg;
  reg [31:0] gpio1_dataOut_reg;

  // Output assignments
  assign uart0_ctrl_en = uart0_ctrl_en_reg;
  assign uart0_ctrl_loopback = uart0_ctrl_loopback_reg;
  assign uart0_data_txData = uart0_data_txData_reg;
  assign gpio0_ctrl_en = gpio0_ctrl_en_reg;
  assign gpio0_dir = gpio0_dir_reg;
  assign gpio0_dataOut = gpio0_dataOut_reg;
  assign gpio1_ctrl_en = gpio1_ctrl_en_reg;
  assign gpio1_dir = gpio1_dir_reg;
  assign gpio1_dataOut = gpio1_dataOut_reg;

  // Write triggers
  assign uart0_data_txData_trg = wr_access && (paddr == 32'h41003008);

  // Read triggers
  assign uart0_data_rxData_trg = rd_access && (paddr == 32'h41003008);

  always_ff @(posedge clock) begin // APB phases
    if (reset) begin
      wr_access <= 1'b0;
      rd_access <= 1'b0;
    end else begin
      wr_access <= wr_access ? 1'b0 : psel && pwrite;
      rd_access <= rd_access ? 1'b0 : psel && !pwrite;
    end
  end

  always_ff @(posedge clock) begin // Register reset and writes
    if (reset) begin
      uart0_ctrl_en_reg <= 1'h0;
      uart0_ctrl_loopback_reg <= 1'h0;
      gpio0_ctrl_en_reg <= 1'h0;
      gpio0_dir_reg <= 32'h0;
      gpio0_dataOut_reg <= 32'h0;
      gpio1_ctrl_en_reg <= 1'h0;
      gpio1_dir_reg <= 32'h0;
      gpio1_dataOut_reg <= 32'h0;
    end else begin
      if (wr_access) begin
        case (paddr)
          32'h41003000: begin
            uart0_ctrl_en_reg <= pwdata[0:0];
            uart0_ctrl_loopback_reg <= pwdata[1:1];
          end
          32'h41003008: uart0_data_txData_reg <= pwdata[7:0];
          32'h41004000: gpio0_ctrl_en_reg <= pwdata[0:0];
          32'h41004004: gpio0_dir_reg <= pwdata[31:0];
          32'h4100400C: gpio0_dataOut_reg <= pwdata[31:0];
          32'h41004010: gpio1_ctrl_en_reg <= pwdata[0:0];
          32'h41004014: gpio1_dir_reg <= pwdata[31:0];
          32'h4100401C: gpio1_dataOut_reg <= pwdata[31:0];
        endcase
      end
    end
  end

  always_comb begin // APB error handling
    if (psel) begin
      if (rd_access) begin
        case (paddr)
          32'h41003000, 32'h41003004, 32'h41003008, 32'h41004000, 32'h41004004, 32'h41004008, 32'h4100400C, 32'h41004010, 32'h41004014, 32'h41004018, 32'h4100401C, 32'h80000000: pslverr = 1'b0;
          default: pslverr = 1'b1;
        endcase
      end else if (wr_access) begin
        case (paddr)
          32'h41003000, 32'h41003008, 32'h41004000, 32'h41004004, 32'h4100400C, 32'h41004010, 32'h41004014, 32'h4100401C: pslverr = 1'b0;
          default: pslverr = 1'b1;
        endcase
      end
    end
  end

  always_comb begin // Read Access
    prdata = 32'h00000000;
    if (psel) begin
      case (paddr)
        32'h41003000: begin
          prdata[0:0] = uart0_ctrl_en_reg;
          prdata[1:1] = uart0_ctrl_loopback_reg;
        end
        32'h41003004: begin
          prdata[0:0] = uart0_status_txEmpty;
          prdata[1:1] = uart0_status_rxReady;
        end
        32'h41003008: prdata[7:0] = uart0_data_rxData;
        32'h41004000: prdata[0:0] = gpio0_ctrl_en_reg;
        32'h41004004: prdata[31:0] = gpio0_dir_reg;
        32'h41004008: prdata[31:0] = gpio0_dataIn;
        32'h4100400C: prdata[31:0] = gpio0_dataOut_reg;
        32'h41004010: prdata[0:0] = gpio1_ctrl_en_reg;
        32'h41004014: prdata[31:0] = gpio1_dir_reg;
        32'h41004018: prdata[31:0] = gpio1_dataIn;
        32'h4100401C: prdata[31:0] = gpio1_dataOut_reg;
        32'h80000000: prdata[31:0] = 32'hDEADBEEF; // sysInfo_id
      endcase
    end
  end

  

endmodule
